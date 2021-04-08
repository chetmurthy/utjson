open Asttools
open Ututil
open Utypes

module Env = struct

type member_t =
  | D of (string * utype_t) list * t
  | RECD of (string * utype_t) list * t
  | M of string * member_t list
and t = member_t list

let refs_of_utype t =
  let rec rerec_utype acc = function
      Simple _ -> acc
  | And (t1,t2) -> rerec_utype (rerec_utype acc t1) t2
  | Or (t1,t2) -> rerec_utype (rerec_utype acc t1) t2
  | Not t -> rerec_utype acc t
  | Atomic l -> List.fold_left rerec_atomic acc l
  | Ref (l,id) -> (l,id)::acc
  and rerec_atomic acc = function
    Field (_, t) -> rerec_utype acc t
  | FieldRE (_, t) -> rerec_utype acc t
  | FieldRequired _ -> acc
  | ArrayOf t -> rerec_utype acc t
  | ArrayTuple l -> List.fold_left rerec_utype acc l
  | ArrayUnique -> acc
  | ArrayIndex (_, t) -> rerec_utype acc t
  | Size _ 
  | StringRE _
  | NumberBound _
  | Sealed _ -> acc
  | OrElse t -> rerec_utype acc t
  | MultipleOf _ -> acc
  in rerec_utype [] t

let lookup1 env s =
  let rec lrec = function
      [] -> Fmt.(failwithf "lookup1: id %s not found in env" s)
    | (D (l, env'))::t -> begin match List.assoc s l with
          v -> Left(v,env')
        | exception Not_found -> lrec t
      end
    | ((RECD (l, env'))::t) as t0 -> begin match List.assoc s l with
          v -> Left(v,((RECD (l, env')))::env')
        | exception Not_found -> lrec t
      end
    | (M(id', l))::t ->
      if s = id' then 
        Right l
      else lrec t
  in
  lrec env

let rec lookup env = function
    [s] -> begin match lookup1 env s with
        Left v -> v
      | Right _ -> Fmt.(failwithf "lookup: lookup of %s yielded module-environment" s)
    end
  | s::t -> begin match lookup1 env s with
        Left _ -> Fmt.(failwithf "lookup: lookup of %s yielded binding (should be module-env)" s)
      | Right ml -> lookup ml t
    end

let check_closed_under env t =
  let refs = refs_of_utype t in
  refs |> List.iter (fun (l,id) ->
      let l = l@[id] in
      ignore(lookup env l)
    )

let elab env t =
  let rec elabrec (env,segacc) = function
      StTypes(true, l) ->
      let m = (RECD (l, env)) in
      List.iter (check_closed_under (m::env)) (List.map snd l) ;
      (m::env, m::segacc)

    | StTypes(false, l) ->
      List.iter (check_closed_under env) (List.map snd l) ;
      let m = (D (l, env)) in
      (m::env, m::segacc)

    | StModuleBinding(mid, Struct l) ->
      let (env', segacc') = List.fold_left elabrec (env,[]) l in
      let m = M(mid, segacc') in
      (m::env, m::segacc')

    | StImport(url,s) ->
      let t = Utconv.load_file url in
      elabrec (env, segacc) (StModuleBinding (s, Struct [t]))

    | StLocal(l1, l2) ->
      let (env1, _) = List.fold_left elabrec (env, []) l1 in
      let (env2, segacc') = List.fold_left elabrec (env1, []) l2 in
      (env2, segacc' @ segacc)
  in
  fst (elabrec (env, []) t)
end

let re_match re s =
  match Str.(search_forward (regexp re) s 0) with
    _ -> true
  | exception Not_found -> false

let rec validate (j : Yojson.Basic.t) env = function
    Simple bty ->
    (match (bty, j) with
       (JNull, `Null) -> true
     | (JString, `String _) -> true
     | (JBool, `Bool _) -> true
     | (JNumber, (`Float _|`Int _)) -> true
     | (JArray, `List _) -> true
     | (JObject, `Assoc _) -> true
     | _ -> false)
  | And(t1, t2) -> validate j env t1 && validate j env t2
  | Or(t1, t2) -> validate j env t1 || validate j env t2
  | Not t -> not (validate j env t)
  | Ref(l, id) ->
    let (t,env) = Env.lookup env (l@[id]) in
    validate j env t
  | Atomic l ->
    List.for_all (validate_atomic j env) l

and validate_atomic j env uty = match (j, uty) with
    (`Assoc l, Field (fname, fty)) -> begin match List.assoc fname l with
        v -> validate v env fty
      | exception Not_found -> true
    end
  | (_, Field _) -> false

  | (`Assoc l, FieldRE (re, t)) ->
    l |> List.for_all (fun (k,v) ->
        if re_match re k then validate v env t else true
      )

  | (_, FieldRE _) -> false
  | (`Assoc l, FieldRequired flds) ->
    flds |> List.for_all (fun f -> List.mem_assoc f l)
  | (_, FieldRequired _) -> false

  | (`List l, ArrayOf uty) ->
    l |> List.for_all (fun j' -> validate j' env uty)
  | (_, ArrayOf uty) -> false

  | (`List l, ArrayTuple utl) ->
    List.for_all2 (fun j' uty' -> validate j' env uty') l utl
  | (_ , ArrayTuple _) -> false

  | (`List l, ArrayUnique) -> List.length l = List.length (uniquize l)
  | (_, ArrayUnique) -> false

  | (`List l, ArrayIndex (n, uty')) ->
    0 <= n && n < List.length l &&
    validate (List.nth l n) env uty'
  | (_, ArrayIndex _) -> false

  | (_, Size _) -> Fmt.(failwithf "Size: unimplemented")
  | (`String s, StringRE re) -> re_match re s
  | (_, NumberBound _) -> Fmt.(failwithf "NumberBound: unimplemented")
  | (_, Sealed _) -> Fmt.(failwithf "Sealed: unimplemented")
  | (_, OrElse _) -> Fmt.(failwithf "OrElse: unimplemented")
  | (`Float n, MultipleOf m) ->
    let (frac, integral) = Float.modf (n /. m) in
    0.0 = frac

  | (`Int n, MultipleOf m) ->
    let (frac, integral) = Float.modf m in
    0.0 = frac && n mod (Float.to_int integral) = 0

  | (_, MultipleOf _) -> false

let env0 = Env.elab [] (StImport ("lib/predefined.utj", "Predefined"))

let elab t = Env.elab env0 t
