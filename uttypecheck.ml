open Asttools
open Pa_ppx_utils.Std

open Ututil
open Utypes
open Utmigrate
open Utio

let load_file ?(with_predefined=false) s =
  let open Fpath in
  if has_ext "json" (v s) then
    Fmt.(failwithf "load_file: should only be called on UTJ files, not JSON: %s" s) ;

  let stl =
    if has_ext "utj" (v s) then
      Utparse0.(parse_file parse_structure) s
    else Fmt.(failwithf "load_file: format not recognized: %s" s) in
  if with_predefined then
  [StImport("lib/predefined.utj", ID.of_string "Predefined")]@stl
  else stl

module TEnv = struct
  type t = (bool, module_type_t, module_type_t) Env.t
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

module FMV = struct
  let rec module_type env = function
      MtSig l ->
      let (_, fv) = signature env l in
      fv
  | MtFunctorType ((mid, argty), resty) ->
    let arg_fv = module_type env argty in
    let res_fv = module_type (mid::env) resty in
    arg_fv@res_fv
  | MtPath (None, _) -> []
  | MtPath(Some mp, _) -> module_path env mp

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
  | SiModuleBinding(mid, mty) -> (env, module_type env mty)
  | SiModuleType (mid, mty) -> (env, module_type env mty)
  | SiInclude mp -> (env, module_path env mp)

let closed_over f x l = 
  x
  |> f []
  |> uniquize
  |> List.for_all (fun x -> List.mem x l)

let closed f x = closed_over f x []
end

let rec lookup_module env = function
    REL h -> TEnv.lookup_module env h
  | TOP _ as p -> Fmt.(failwithf "TOP should never appear in a module_path here: %a" pp_module_path_t p)
  | DEREF (p, id) -> begin match lookup_module env p with
        MtSig l -> begin match l |> List.find_map (function
            SiModuleBinding(h', mty) when id=h' -> Some mty
          | _ -> None) with
          None -> Fmt.(failwith "lookup_module: cannot resolve, env lookup failure" pp_module_path_t p)
        | Some mty -> mty
        end
      | mty -> Fmt.(failwith "lookup_module: path %a resolves to type %a" pp_module_path_t p pp_module_type_t mty)
    end
    

let lookup_module_type env p =
  match p with
    (None, h) -> TEnv.lookup_module_type env h
  | (Some mp, h) ->
    match lookup_module env mp with
      MtSig l -> begin
        match l |> List.find_map (function
              SiModuleType(h', mty) when h=h' -> Some mty
            | _ -> None) with
          None -> Fmt.(failwithf "lookup_module_type: cannot resolve, env lookup failure: %a.%s" pp_module_path_t mp (ID.to_string h))
        | Some mty ->
          mty
      end
    | mty -> Fmt.(failwith "lookup_module_type: path %a resolves to type %a" pp_module_path_t mp pp_module_type_t mty)


let satisfies_constraint env ~lhs ~rhs =
  match (lhs, rhs) with
  (MtSig lhsl, MtSig rhsl) ->
  rhsl |> List.for_all (function
        SiInclude _ as si -> Fmt.(failwithf "satisfies_constraint: internal error: %a" pp_sig_item_t si)
      | si -> List.mem si lhsl)

  | _ -> Fmt.(failwithf "satisfies_constraint: malformed args: lhs=%a rhs=%a" pp_module_type_t lhs pp_module_type_t rhs)

let fresh s =
  let open Str in
  if string_match (regexp "^\\(.*[^0-9]\\)\\([0-9]*\\)$") s 0 then
    let prefix = matched_group 1 s and
    num = matched_group 2 s in
    let num = if num = "" then -1 else int_of_string num in
    prefix^(string_of_int (num+1))
  else Fmt.(failwithf "fresh: internal error")

let rec tc_utype env ut = match ut with
    Ref (None, t) as ut -> begin match Env.lookup_t env t with
        None -> Fmt.(failwith "tc_utype: id %s not found in type-environment" (ID.to_string t))
      | Some sealed -> (ut, sealed)
    end
  | Ref (Some mpath, t) as ut -> begin match lookup_module env mpath with
        MtSig l -> begin match l |> List.find_map (function
            SiType (t', sealed) when t = t' -> Some sealed
          | _ -> None
        ) with
          Some sealed -> (ut, sealed)
        | None ->
          Fmt.(failwithf "tc_utype: utype %s not found in environment" (Normal.printer ut))
        end
      | _ -> Fmt.(failwithf "tc_utype: module-path %a did not yield a signature" pp_module_path_t mpath)
    end
    
  | UtTrue -> (ut, false)
  | UtFalse -> (ut, false)
  | Simple bt -> (Simple bt, false)
  | And(ut1, ut2) ->
    let (ut1, sealed1) = tc_utype env ut1 in
    let (ut2, sealed2) = tc_utype env ut2 in
    if sealed1=sealed2 then (And(ut1, ut2), sealed1)
    else Fmt.(failwithf "tc_utype: cannot mix sealed/unsealed types in conjunction: %s"
                (Normal.printer ut))
           
  | Or(ut1, ut2) ->
    let (ut1, sealed1) = tc_utype env ut1 in
    let (ut2, sealed2) = tc_utype env ut2 in
    if sealed1=sealed2 then (Or(ut1, ut2), sealed1)
    else Fmt.(failwithf "tc_utype: cannot mix sealed/unsealed types in disjunction: %s"
                (Normal.printer ut))
           
  | Xor(ut1, ut2) ->
    let (ut1, sealed1) = tc_utype env ut1 in
    let (ut2, sealed2) = tc_utype env ut2 in
    if sealed1=sealed2 then (Xor(ut1, ut2), sealed1)
    else Fmt.(failwithf "tc_utype: cannot mix sealed/unsealed types in xor: %s"
                (Normal.printer ut))
           
  | Impl(ut1, ut2) ->
    let (ut1, sealed1) = tc_utype env ut1 in
    let (ut2, sealed2) = tc_utype env ut2 in
    if sealed1=sealed2 then (Impl(ut1, ut2), sealed1)
    else Fmt.(failwithf "tc_utype: cannot mix sealed/unsealed types in implication: %s"
                (Normal.printer ut))
           
  | Not ut1 ->
    let (ut1, sealed1) = tc_utype env ut1 in
      (Not ut1, sealed1)
        
  | Atomic l ->
    let l = List.map (tc_atomic_type env) l in
    (Atomic l, false)
    
  | Seal(ut1, l, orelse) ->
    let (ut1, sealed1) = tc_utype env ut1 in
    if sealed1 then
      Fmt.(failwithf "tc_utype: pointless to seal an already-sealed type: %s"
             (Normal.printer ut))
    else
      let l = List.map (fun (re, ut) -> (re, tc_sub_utype env ut)) l in
      let orelse = Option.map (tc_sub_utype env) orelse in
      (Seal(ut1,l,orelse), true)

and tc_sub_utype env ut = fst(tc_utype env ut)

and tc_atomic_type env ty = match ty with
    Field(fname, ut) -> Field(fname, tc_sub_utype env ut)
  | FieldRE(re, ut) ->  FieldRE(re, tc_sub_utype env ut)
  | FieldRequired _ -> ty
  | ArrayOf ut -> ArrayOf(tc_sub_utype env ut)
  | ArrayTuple l -> ArrayTuple(List.map (tc_sub_utype env) l)
  | ArrayUnique -> ty
  | ArrayIndex (n, ut) -> ArrayIndex (n, tc_sub_utype env ut)
  | Size _ -> ty
  | StringRE _ -> ty
  | NumberBound _ -> ty
  | Sealed -> ty
  | OrElse ut -> OrElse (tc_sub_utype env ut)
  | MultipleOf _ -> ty
  | Enum _ -> ty
  | Default _ -> ty
  | Format _ -> ty
  | PropertyNames ut -> PropertyNames(tc_sub_utype env ut)
  | ContentMediaType _ -> ty
  | ContentEncoding _ -> ty

and tc_struct_item env = function
    StTypes (recflag,l) ->
    let subenv = if recflag then
        l |> List.fold_left (fun env (tid, sealed, _) -> TEnv.push_type env (tid, sealed))
          env
    else env in
    let l =
      l |> List.rev_map (fun (tid, sealed, ut) ->
          let (ut, sealed') = tc_utype subenv ut in
          if sealed <> sealed' then
            Fmt.(failwithf "tc_struct_item: declared status of type %s was %s, but type-checker inferred %s"
                   (ID.to_string tid)
                   (if sealed then "sealed" else "unsealed")
                   (if sealed' then "sealed" else "unsealed"))
          else
            (tid, sealed, ut)) |> List.rev in
    let newenv = List.fold_left (fun env (tid, sealed, _) -> TEnv.push_type env (tid, sealed)) env l in
    (newenv,
     (StTypes(recflag, l),
     l |> List.map (fun (tid, sealed, _) -> SiType (tid, sealed))))

  | StModuleBinding (mid, me) ->
    let (me, mty) = tc_module_expr env me in
    let me = match mty with
        MtSig _ -> MeCast (me, mty)
      | _ -> me in
    let st = StModuleBinding (mid, me) in
    let si = SiModuleBinding(mid, mty) in
    (TEnv.push_module env (mid, mty), (st, [si]))

  | StImport(fname, mid) ->
    let stl = load_file fname in
    let st = StModuleBinding (mid, MeStruct stl) in
    tc_struct_item env st

  | StLocal (stl1, stl2) ->
    let (env',(stl1, _)) = tc_structure env stl1 in
    let (_, (stl2, sil2)) = tc_structure env' stl2 in
    let (env, sil2) = tc_signature env sil2 in
    let st = StLocal (stl1, stl2) in
    (env, (st, sil2))

  | StOpen (p, formal_mty) as st ->
    let formal_mty = Option.map (tc_module_type env) formal_mty in
    let actual_mty = lookup_module env p in
    formal_mty |> Option.iter (fun formal_mty ->
        if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
          Fmt.(failwithf "tc_struct_item: open (%a : %a) does not satisfy %a"
                 pp_module_path_t p pp_module_type_t actual_mty pp_module_type_t formal_mty)) ;
    let mty = match formal_mty with Some mty -> mty | None -> actual_mty in
    let st = StOpen (p, Some mty) in
    begin match mty with
        MtSig l ->
        let (env, _) = tc_signature env l in
        (env, (st,[]))
        
      | _ -> Fmt.(failwith "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StInclude (p, formal_mty) as st ->
    let formal_mty = Option.map (tc_module_type env) formal_mty in
    let actual_mty = lookup_module env p in
    formal_mty |> Option.iter (fun formal_mty ->
        if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
          Fmt.(failwithf "tc_struct_item: include (%a : %a) does not satisfy %a"
                 pp_module_path_t p pp_module_type_t actual_mty pp_module_type_t formal_mty)) ;
    let mty = match formal_mty with Some mty -> mty | None -> actual_mty in
    let st = StInclude (p, Some mty) in
    begin match mty with
        MtSig l ->
        let (env, sil) = tc_signature env l in
        (env, (st, sil))

      | _ -> Fmt.(failwith "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StModuleType (id, mty) ->
    let mty = tc_module_type env mty in
    let st = StModuleType (id, mty) in
    (TEnv.push_module_type env (id, mty), (st, [SiModuleType (id, mty)]))

and tc_structure env l =
  let (env, stacc, siacc) = List.fold_left (fun (env,stacc, siacc) st ->
      let (env', (st, sil)) = tc_struct_item env st in
      (env', st::stacc, sil::siacc))
      (env, [], []) l in
  (env, (List.rev stacc, List.concat (List.rev siacc)))

and tc_signature env l =
  let (env, acc) = List.fold_left (fun (env,acc) si ->
      let (env', sil) = tc_sig_item env si in
      (env', sil::acc))
    (env,[]) l in
  (env, List.concat (List.rev acc))
  
and tc_sig_item env = function
    SiType (s, sealed) as si -> (TEnv.push_type env (s,sealed), [si])
  | SiModuleBinding (mid, mty) ->
    let mty = tc_module_type env mty in
    (TEnv.push_module env (mid, mty), [SiModuleBinding (mid, mty)])
  | SiModuleType (mid, mty) ->
    let mty = tc_module_type env mty in
    (TEnv.push_module_type env (mid, mty), [SiModuleType (mid, mty)])
  | SiInclude p ->
    let mty = lookup_module env p in begin match mty with
        MtSig l ->
        tc_signature env l
      | _ -> Fmt.(failwith "tc_sig_item: cannot typecheck %a, path does not denote a module" pp_module_path_t p)
    end


and tc_module_expr env = function
    MeStruct l ->
    let (_, (stl, sil)) = tc_structure env l in
    (MeStruct stl, MtSig (List.sort_uniq Stdlib.compare sil))

  | MeFunctorApp(me1,me2) as me -> begin match (tc_module_expr env me1, tc_module_expr env me2) with
      ((me1, MtFunctorType((mid, formal_mty), resmty)), (me2, actual_mty)) ->
      let resmty = tc_module_type (TEnv.push_module env (mid, formal_mty)) resmty in
      if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
        Fmt.(failwithf "functor-application fails: argument (%a : %a) does not satisfy %a"
               pp_module_expr_t me2 pp_module_type_t actual_mty pp_module_type_t formal_mty) ;
      (MeFunctorApp(me1,me2), resmty)

      | ((_, lhs), _) -> Fmt.(failwithf "tc_module_expr: type of lhs not a functor-type %a" pp_module_type_t lhs)
    end

  | MePath p as st ->
    (st, lookup_module env p)

  | MeFunctor((mid, argmty), me) ->
    let argmty = tc_module_type env argmty in
    let (me, resmty) = tc_module_expr (TEnv.push_module env (mid, argmty)) me in
    let st = MeFunctor((mid, argmty), me) in
    (st, MtFunctorType((mid, argmty), resmty))

  | MeCast(me, formal_mty) ->
    let formal_mty = tc_module_type env formal_mty in
    let (me, actual_mty) = tc_module_expr env me in
    if not (satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty) then
      Fmt.(failwithf "module-expr cast fails: module_expr (%a : %a) does not satisfy %a"
             pp_module_expr_t me pp_module_type_t actual_mty pp_module_type_t formal_mty) ;
    (MeCast(me, formal_mty), formal_mty)

and tc_module_type env mt =
  let mt' = tc_module_type0 env mt in
  if not FMV.(closed module_type mt') then
    Fmt.(failwithf "tc_module_type: result was not closed over module vars:\nenv: %a\nmt (original): %a\nmt (after tc): %a\n"
           TEnv.pp env pp_module_type_t mt pp_module_type_t mt') ;
  mt'

and tc_module_type0 env = function
    MtSig l ->
    let (_, l) = tc_signature env l in
    MtSig (List.sort_uniq Stdlib.compare l)
  | MtFunctorType ((mid, argty), resty) ->
    let argty = tc_module_type env argty in
    let resty = tc_module_type (TEnv.push_module env (mid, argty)) resty in
    MtFunctorType ((mid, argty), resty)
  | MtPath (popt, id) ->
    lookup_module_type env (popt,id)

