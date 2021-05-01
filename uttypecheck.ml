open Asttools
open Pa_ppx_utils.Std

open Ututil
open Utypes
open Utio

module Reloc = struct
  open Utmigrate.Self

  let wrap_cmp ?(reloc=(fun _ -> Ploc.dummy)) f relocf a b = f (relocf reloc a) (relocf reloc b)
  open Utmigrate

  let structure reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_structure dt x

  let module_expr_t reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_module_expr_t dt x

  let module_type_t reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_module_type_t dt x

  let utype_t reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_utype_t dt x

  let struct_item_t reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_struct_item_t dt x

  let sig_item_t reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_sig_item_t dt x

  let signature reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_signature dt x

  let top_bindings reloc x =
    let dt = make_dt () in
    let new_migrate_loc dt loc = reloc loc in
    let dt = { dt with migrate_loc = new_migrate_loc } in
    dt.migrate_top_bindings dt x
end

let load_file ?(with_predefined=false) s =
  let open Fpath in
  if has_ext "json" (v s) then
    Fmt.(failwithf "load_file: should only be called on UTJ files, not JSON: %s" s) ;

  let stl =
    if has_ext "utj" (v s) then
      Utparse0.(parse_file parse_structure) s
    else Fmt.(failwithf "load_file: format not recognized: %s" s) in
  if with_predefined then
    let loc = match stl with [] -> Ploc.dummy | h::t -> loc_of_struct_item h in
    [StImport(loc, "lib/predefined.utj", ID.of_string "Predefined")]@stl
  else stl

module TEnv = struct
  type t = (AN.t, module_type_t, module_type_t) Env.t
  [@@deriving show { with_path = false },eq]

  let mt = Env.mk ()
  let push_type t (id, sealed) = Env.add_t t (id,sealed)
  let push_module t (id, mty) = Env.add_m t (id, mty)
  let push_module_type t (id, mty) = Env.add_mt t (id, mty)

  let lookup_type t name = match Env.lookup_t t name with
      Some v -> v
    | None -> Fmt.(failwithf "Env.lookup_type: cannot find type %s in type-environment" (ID.to_string name))

  let lookup_module t name = match Env.lookup_m t name with
      Some v -> v
    | None -> Fmt.(failwithf "Env.lookup_module: cannot find module id %s in type-environment" (ID.to_string name))
  let lookup_module_type t name = match Env.lookup_mt t name with
      Some v -> v
    | None -> Fmt.(failwithf "Env.lookup_module: cannot find module_type id %s in type-environment" (ID.to_string name))
end

module TCtxt = struct
  type t = { sealed_context : bool }
  let mk sealed_context = { sealed_context }
end

module FMV = struct
  let rec module_type env = function
      MtSig (_, l) ->
      let (_, fv) = signature env l in
      fv
  | MtFunctorType (_, (mid, argty), resty) ->
    let arg_fv = module_type env argty in
    let res_fv = module_type (mid::env) resty in
    arg_fv@res_fv
  | MtPath (_, (None, _)) -> []
  | MtPath(_, (Some mp, _)) -> module_path env mp

  and module_path env = function
      REL id  ->
      if List.mem id env then [] else [id]
    | TOP _ as mp -> Fmt.(failwith "FMV.module_path: internal error %a" pp_module_path_t mp)
    | DEREF (mp, _) -> module_path env mp

  and signature env sil = 
    List.fold_left (fun (env, fv) si ->
        let (env, si_fv) = sig_item env si in
        (env, si_fv@fv)
      ) (env, []) sil

and sig_item env = function
    SiType _ -> (env, [])
  | SiModuleBinding(_, mid, mty) -> (env, module_type env mty)
  | SiModuleType (_, mid, mty) -> (env, module_type env mty)
  | SiInclude (_, mp) -> (env, module_path env mp)

let closed_over f x l = 
  x
  |> f []
  |> uniquize
  |> List.for_all (fun x -> List.mem x l)

let closed f x = closed_over f x []
end

module ANO = struct
open AN

(** a type is unsealable if it is not sealed, and its base_types do
   not include "object", "array".  *)
let unsealable = function
    SEALED -> false
  | UNSEALED l -> [] = intersect [JObject; JArray] l

let sealed_or_unsealable = function
    SEALED -> true
  | UNSEALED _ as a -> unsealable a

  (** two types can be conjoined (&&) if:

    (1) they are both sealed

      the result is sealed

    (2) they are both unsealed

      the result is unsealed, and the base_types are intersected

  *)

let conjoin loc a1 a2 =
  match (a1, a2) with
    SEALED, SEALED -> SEALED
  | UNSEALED l1, UNSEALED l2 ->
    let l = canon (intersect l1 l2) in
    if l = [] then Fmt.(raise_failwithf loc "AnnotationOps.conjoin: cannot conjoin (&&) %a, %a: empty unsealed result"
           pp a1 pp a2) ;
    AN.mk ~base_types:l false
  | _ ->
    Fmt.(raise_failwithf loc "AnnotationOps.conjoin: cannot conjoin (&&) %a, %a"
           pp a1 pp a2)

(** two types can be disjoined if

    (1) they are both sealed

    result is sealed

    (2) they are both unsealed

    result intersects their base_types (and it MUST be nonempty)

    (3) one is sealed, and the other is unsealable

    result is sealed

*)

let disjoin loc a1 a2 =
  match (a1, a2) with
    SEALED, SEALED -> SEALED
  | UNSEALED l1, UNSEALED l2 ->
    AN.mk ~base_types:(canon (l1@l2)) false
  | SEALED, _ when unsealable a2 -> SEALED
  | _, SEALED when unsealable a1 -> SEALED
  | _ ->
    Fmt.(raise_failwithf loc "AnnotationOps.disjion: cannot disjoin (||) %a, %a"
           pp a1 pp a2)

(** two types can be xored if

    either or both types is sealed or unsealable

    if either type is sealed, then result is sealed
    otherwise, union the base_types.

*)

let xor loc a1 a2 =
  let open AN in
  if sealed_or_unsealable a1 && sealed_or_unsealable a2 then
    match (a1, a2) with
      (SEALED, _) | (_, SEALED) -> SEALED
    | (UNSEALED l1, UNSEALED l2) -> AN.mk ~base_types:(canon (l1@l2)) false
  else
    Fmt.(raise_failwithf loc "AnnotationOps.xor: cannot xor %a, %a"
           pp a1 pp a2)

(** two types can be implicated (a => b) if

    either or both types is sealed or unsealable

    result is sealed

*)

let impl loc a1 a2 =
  if sealed_or_unsealable a1 && sealed_or_unsealable a2 then
    SEALED
  else
    Fmt.(raise_failwithf loc "AnnotationOps.impl: cannot => %a, %a"
           pp a1 pp a2)

(** a type can be negated (not a) if

    the type is sealed or unsealable

    result is sealed

*)

let neg loc a1 =
  if sealed_or_unsealable a1 then
    SEALED
  else
    Fmt.(raise_failwithf loc "AnnotationOps.neg: cannot negate %a"
           pp a1)

end

let rec lookup_module loc env = function
    REL h -> TEnv.lookup_module env h
  | TOP _ as p -> Fmt.(failwithf "TOP should never appear in a module_path here: %a" pp_module_path_t p)
  | DEREF (p, id) -> begin match lookup_module loc env p with
        MtSig (_, l) -> begin match l |> List.find_map (function
            SiModuleBinding(_, h', mty) when id=h' -> Some mty
          | _ -> None) with
          None -> Fmt.(raise_failwithf loc "lookup_module: cannot resolve, env lookup failure: %a" pp_module_path_t p)
        | Some mty -> mty
        end
      | mty -> Fmt.(raise_failwithf loc "lookup_module: path %a resolves to type %a" pp_module_path_t p pp_module_type_t mty)
    end
    

let lookup_module_type loc env p =
  match p with
    (None, h) -> TEnv.lookup_module_type env h
  | (Some mp, h) ->
    match lookup_module loc env mp with
      MtSig (_, l) -> begin
        match l |> List.find_map (function
              SiModuleType(_, h', mty) when h=h' -> Some mty
            | _ -> None) with
          None -> Fmt.(raise_failwithf loc "lookup_module_type: cannot resolve, env lookup failure: %a.%s" pp_module_path_t mp (ID.to_string h))
        | Some mty ->
          mty
      end
    | mty -> Fmt.(raise_failwithf loc "lookup_module_type: path %a resolves to type %a" pp_module_path_t mp pp_module_type_t mty)


let mem_signature si sil =
  None <> (sil |> List.find_opt (Reloc.(wrap_cmp Debug.sig_item_cmp sig_item_t) si))

let satisfies_constraint env ~lhs ~rhs =
  match (lhs, rhs) with
  (MtSig (_, lhsl), MtSig (loc, rhsl)) ->
  rhsl |> List.for_all (function
        SiInclude _ as si -> Fmt.(raise_failwithf loc "satisfies_constraint: internal error: %a" pp_sig_item_t si)
      | si -> mem_signature si lhsl)

  | _ -> Fmt.(raise_failwithf (loc_of_module_type rhs) "satisfies_constraint: malformed args: lhs=%a rhs=%a" pp_module_type_t lhs pp_module_type_t rhs)

let fresh s =
  let open Str in
  if string_match (regexp "^\\(.*[^0-9]\\)\\([0-9]*\\)$") s 0 then
    let prefix = matched_group 1 s and
    num = matched_group 2 s in
    let num = if num = "" then -1 else int_of_string num in
    prefix^(string_of_int (num+1))
  else Fmt.(failwithf "fresh: internal error")

let infer_base_type = function
  | `Null -> JNull
  | `Bool _ -> JBool
  | `Int _ | `Float _ -> JNumber
  | `String _ -> JString
  | `Assoc _ -> JObject
  | `List _ -> JArray

let rec tc_utype env ut = match ut with
    Ref (loc, (None, t)) as ut -> begin match Env.lookup_t env t with
        None -> Fmt.(raise_failwithf loc "tc_utype: id %s not found in type-environment" (ID.to_string t))
      | Some anno -> (ut, anno)
    end
  | Ref (loc, (Some mpath, t)) as ut -> begin match lookup_module loc env mpath with
        MtSig (_, l) -> begin match l |> List.find_map (function
            SiType (_, t', anno) when t = t' -> Some anno
          | _ -> None
        ) with
          Some sealed -> (ut, sealed)
        | None ->
          Fmt.(raise_failwithf loc "tc_utype: utype %s not found in environment" (Normal.printer ut))
        end
      | _ -> Fmt.(raise_failwithf loc "tc_utype: module-path %a did not yield a signature" pp_module_path_t mpath)
    end
    
  | UtTrue _ -> (ut, AN.mk ~base_types:all_base_types false)
  | UtFalse _ -> (ut, AN.mk ~base_types:all_base_types false)
  | Simple (loc, bt) -> let (bt, anno) = tc_base_type env bt in (Simple (loc, bt), anno)
  | And(loc, ut1, ut2) ->
    let (ut1, anno1) = tc_utype env ut1 in
    let (ut2, anno2) = tc_utype env ut2 in
    (And(loc, ut1, ut2), ANO.conjoin loc anno1 anno2)
           
  | Or(loc, ut1, ut2) ->
    let (ut1, anno1) = tc_utype env ut1 in
    let (ut2, anno2) = tc_utype env ut2 in
    (Or(loc, ut1, ut2), ANO.disjoin loc anno1 anno2)
           
  | Xor(loc, ut1, ut2) ->
    let (ut1, anno1) = tc_utype env ut1 in
    let (ut2, anno2) = tc_utype env ut2 in
    (Xor(loc, ut1, ut2), ANO.xor loc anno1 anno2)
           
  | Impl(loc, ut1, ut2) ->
    let (ut1, anno1) = tc_utype env ut1 in
    let (ut2, anno2) = tc_utype env ut2 in
    (Xor(loc, ut1, ut2), ANO.impl loc anno1 anno2)
           
  | Not (loc, ut1) ->
    let (ut1, anno1) = tc_utype env ut1 in
    (Not(loc, ut1), ANO.neg loc anno1)
        
  | Atomic (loc, l) ->
    assert (l <> []) ;
    let tcl = l |> List.rev_map (tc_atomic_type env) |> List.rev in
    let l = List.map fst tcl in
    let annol = List.map snd tcl in
    let anno = List.fold_left (ANO.conjoin loc) (List.hd annol) (List.tl annol) in
    (Atomic (loc, l), anno)

  | Seal(loc, ut1, l, orelse) ->
    let (ut1, anno1) = tc_utype env ut1 in
    if AN.sealed anno1 then
      Fmt.(raise_failwithf loc "tc_utype: pointless to seal an already-sealed type")
    else
      let l = List.map (fun (re, ut) -> (re, tc_sub_utype env ut)) l in
      let orelse = tc_sub_utype env orelse in
      (Seal(loc, ut1,l,orelse), AN.mk true)

and tc_base_type env t = match t with
    JNull | JString | JBool | JNumber -> (t, AN.mk ~base_types:[t] false)
  | JArray | JObject -> (t, AN.mk ~base_types:[t] false)

and tc_sub_utype env ut = fst(tc_utype env ut)

and tc_atomic_type env ty = match ty with
    Field(loc, fname, ut) ->
    (Field(loc, fname, tc_sub_utype env ut), AN.mk ~base_types:[JObject] false)
  | FieldRequired _ ->
    (ty, AN.mk ~base_types:[JObject] false)
  | ArrayOf (loc, ut) ->
    (ArrayOf(loc, tc_sub_utype env ut), AN.mk ~base_types:[JArray] false)
  | ArrayTuple (loc, l) ->
    (ArrayTuple(loc, List.map (tc_sub_utype env) l), AN.mk ~base_types:[JArray] false)
  | ArrayUnique _ ->
    (ty, AN.mk ~base_types:[JArray] false)
  | ArrayIndex (loc, n, ut) ->
    (ArrayIndex (loc, n, tc_sub_utype env ut), AN.mk ~base_types:[JArray] false)
  | Size _ -> (ty, AN.mk ~base_types:[JArray;JString;JObject] false)
  | StringRE _ -> (ty, AN.mk ~base_types:[JString] false)
  | NumberBound _ -> (ty, AN.mk ~base_types:[JNumber] false)
  | MultipleOf _ -> (ty, AN.mk ~base_types:[JNumber] false)
  | Enum (_, l) -> (ty, AN.mk ~base_types:(l |> List.map infer_base_type |> canon) false)
  | Default (_, j) -> (ty, AN.mk ~base_types:[infer_base_type j] false)
  | Format _ -> (ty, AN.mk ~base_types:[JString] false)
  | PropertyNames (loc, ut) ->
    (PropertyNames(loc, tc_sub_utype env ut), AN.mk ~base_types:[JObject] false)
  | ContentMediaType _ -> (ty, AN.mk ~base_types:[JString] false)
  | ContentEncoding _ -> (ty, AN.mk ~base_types:[JString] false)

and tc_struct_item env = function
    StTypes (loc, recflag,l) ->
    let subenv = if recflag then
        l |> List.fold_left (fun env -> function
              (tid, Some anno, _) -> TEnv.push_type env (tid, anno)
            | (tid, None, _) ->
              Fmt.(raise_failwithf loc "tc_struct_item: recursive types (%a) MUST be annotated"
                     ID.pp_hum tid
                  )
          )
          env
    else env in
    let l =
      l |> List.rev_map (fun (tid, anno_opt, ut) ->
          let (ut, anno') = tc_utype subenv ut in
          if None <> anno_opt && anno_opt <> Some anno' then
            Fmt.(raise_failwithf loc "tc_struct_item: declared annotation of type %s was %a, but type-checker inferred %a"
                   (ID.to_string tid)
                   AN.pp_t_option anno_opt
                   AN.pp anno')
          else
            (tid, Some anno', ut)) |> List.rev in
    let newenv = List.fold_left (fun env -> function
          (tid, Some anno, _) -> TEnv.push_type env (tid, anno)
        | (_, None, _) -> assert false
      ) env l in
    (newenv,
     (StTypes(loc, recflag, l),
     l |> List.map (function
            (tid, Some anno, _) -> SiType (loc, tid, anno)
          | (_, None, _) -> assert false
        )))

  | StModuleBinding (loc, mid, me) ->
    let (me, mty) = tc_module_expr env me in
    let me = match mty with
        MtSig _ -> MeCast (loc_of_module_expr me, me, mty)
      | _ -> me in
    let st = StModuleBinding (loc, mid, me) in
    let si = SiModuleBinding(loc, mid, mty) in
    (TEnv.push_module env (mid, mty), (st, [si]))

  | StImport(loc, fname, mid) ->
    let stl = load_file fname in
    let st = StModuleBinding (loc, mid, MeStruct (loc_of_structure stl, stl)) in
    tc_struct_item env st

  | StLocal (loc, stl1, stl2) ->
    let (env',(stl1, _)) = tc_structure env stl1 in
    let (_, (stl2, sil2)) = tc_structure env' stl2 in
    let (env, sil2) = tc_signature env sil2 in
    let st = StLocal (loc, stl1, stl2) in
    (env, (st, sil2))

  | StOpen (loc, p, formal_mty) as st ->
    let formal_mty = Option.map (tc_module_type env) formal_mty in
    let actual_mty = lookup_module loc env p in
    formal_mty |> Option.iter (fun formal_mty ->
        if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
          Fmt.(raise_failwithf loc "tc_struct_item: open (%a : %a) does not satisfy %a"
                 pp_module_path_t p pp_module_type_t actual_mty pp_module_type_t formal_mty)) ;
    let mty = match formal_mty with Some mty -> mty | None -> actual_mty in
    let st = StOpen (loc, p, Some mty) in
    begin match mty with
        MtSig (_, l) ->
        let (env, _) = tc_signature env l in
        (env, (st,[]))
        
      | mt -> Fmt.(raise_failwithf (loc_of_module_type mt) "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StInclude (loc, p, formal_mty) as st ->
    let formal_mty = Option.map (tc_module_type env) formal_mty in
    let actual_mty = lookup_module loc env p in
    formal_mty |> Option.iter (fun formal_mty ->
        if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
          Fmt.(failwithf "tc_struct_item: include (%a : %a) does not satisfy %a"
                 pp_module_path_t p pp_module_type_t actual_mty pp_module_type_t formal_mty)) ;
    let mty = match formal_mty with Some mty -> mty | None -> actual_mty in
    let st = StInclude (loc, p, Some mty) in
    begin match mty with
        MtSig (_, l) ->
        let (env, sil) = tc_signature env l in
        (env, (st, sil))

      | _ -> Fmt.(raise_failwithf loc "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StModuleType (loc, id, mty) ->
    let mty = tc_module_type env mty in
    let st = StModuleType (loc, id, mty) in
    (TEnv.push_module_type env (id, mty), (st, [SiModuleType (loc, id, mty)]))

and thin_signature sil =
  let (sil, _) = List.fold_right (fun si (sil, env) ->
      match si with
        SiType(_, tid, _) when Env.has_t env tid -> (sil, env)
      | SiType(_, tid, _) -> (si::sil, Env.add_t env (tid, ()))
      | SiModuleBinding(_, mid, _) when Env.has_m env mid -> (sil, env)
      | SiModuleBinding(_, mid, _) -> (si::sil, Env.add_m env (mid, ()))
      | SiModuleType(_, mid, _) when Env.has_mt env mid -> (sil, env)
      | SiModuleType(_, mid, _) -> (si::sil, Env.add_mt env (mid, ()))
      | _ -> (sil, env)
    ) sil ([], Env.mk()) in
  sil

and tc_structure env l =
  let (env, stacc, siacc) = List.fold_left (fun (env,stacc, siacc) st ->
      let (env', (st, sil)) = tc_struct_item env st in
      (env', st::stacc, sil::siacc))
      (env, [], []) l in
  let sil = List.concat (List.rev siacc) in
  let sil = thin_signature sil in
  (env, (List.rev stacc, sil))

and tc_signature env l =
  let (env, acc) = List.fold_left (fun (env,acc) si ->
      let (env', sil) = tc_sig_item env si in
      (env', sil::acc))
    (env,[]) l in
  (env, List.concat (List.rev acc))
  
and tc_sig_item env = function
    SiType (loc, s, sealed) as si -> (TEnv.push_type env (s,sealed), [si])
  | SiModuleBinding (loc, mid, mty) ->
    let mty = tc_module_type env mty in
    (TEnv.push_module env (mid, mty), [SiModuleBinding (loc, mid, mty)])
  | SiModuleType (loc, mid, mty) ->
    let mty = tc_module_type env mty in
    (TEnv.push_module_type env (mid, mty), [SiModuleType (loc, mid, mty)])
  | SiInclude (loc, p) ->
    let mty = lookup_module loc env p in begin match mty with
        MtSig (_, l) ->
        tc_signature env l
      | _ -> Fmt.(raise_failwithf loc "tc_sig_item: cannot typecheck %a, path does not denote a module" pp_module_path_t p)
    end


and tc_module_expr env = function
    MeStruct (loc, l) ->
    let (_, (stl, sil)) = tc_structure env l in
    (MeStruct (loc, stl), MtSig (loc, List.sort_uniq Reloc.(wrap_cmp Stdlib.compare sig_item_t) sil))

  | MeFunctorApp(loc, me1,me2) as me -> begin match (tc_module_expr env me1, tc_module_expr env me2) with
      ((me1, MtFunctorType(loc, (mid, formal_mty), resmty)), (me2, actual_mty)) ->
      let resmty = tc_module_type (TEnv.push_module env (mid, formal_mty)) resmty in
      if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
        Fmt.(raise_failwithf loc "functor-application fails: argument (%s : %s) does not satisfy %s"
               (Normal.module_expr_printer me2)
               (Normal.module_type_printer actual_mty)
               (Normal.module_type_printer formal_mty)) ;
      (MeFunctorApp(loc, me1,me2), resmty)

      | ((_, lhs), _) -> Fmt.(raise_failwithf (loc_of_module_type lhs) "tc_module_expr: type of lhs not a functor-type %a" pp_module_type_t lhs)
    end

  | MePath (loc, p) as st ->
    (st, lookup_module loc env p)

  | MeFunctor(loc, (mid, argmty), me) ->
    let argmty = tc_module_type env argmty in
    let (me, resmty) = tc_module_expr (TEnv.push_module env (mid, argmty)) me in
    let st = MeFunctor(loc, (mid, argmty), me) in
    (st, MtFunctorType(loc, (mid, argmty), resmty))

  | MeCast(loc, me, formal_mty) ->
    let formal_mty = tc_module_type env formal_mty in
    let (me, actual_mty) = tc_module_expr env me in
    if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
      Fmt.(raise_failwithf loc "module-expr cast fails: module_expr (%s : %s) does not satisfy %s"
             (Normal.module_expr_printer me)
             (Normal.module_type_printer actual_mty)
             (Normal.module_type_printer formal_mty)
          ) ;
    (MeCast(loc, me, formal_mty), formal_mty)

and tc_module_type env mt =
  let mt' = tc_module_type0 env mt in
  if not FMV.(closed module_type mt') then
    Fmt.(failwithf "tc_module_type: result was not closed over module vars:\nenv: %a\nmt (original): %a\nmt (after tc): %a\n"
           TEnv.pp env pp_module_type_t mt pp_module_type_t mt') ;
  mt'

and tc_module_type0 env = function
    MtSig (loc, l) ->
    let (_, l) = tc_signature env l in
    MtSig (loc, List.sort_uniq Stdlib.compare l)
  | MtFunctorType (loc, (mid, argty), resty) ->
    let argty = tc_module_type env argty in
    let resty = tc_module_type (TEnv.push_module env (mid, argty)) resty in
    MtFunctorType (loc, (mid, argty), resty)
  | MtPath (loc, (popt, id)) ->
    lookup_module_type loc env (popt,id)

