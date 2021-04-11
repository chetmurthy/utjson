open Asttools
open Ututil
open Utypes
open Utmigrate


module Env = struct
  type binding_t =
      Type
    | Module of module_type_t
    | ModuleType of module_type_t
  type t = (string * binding_t) list

  let mt = []
  let push_type env id = (id, Type)::env
  let push_module env (id, mty) = (id, Module mty)::env
  let push_module_type env (id, mty) = (id, ModuleType mty)::env

  let lookup env name = match List.assoc name env with
      v -> v
    | exception Not_found -> Fmt.(failwithf "Env.lookup: cannot find module id %s in type-environment" name)
end

let lookup_type env h =
  match Env.lookup env h with
    Env.Type -> ()
  | _ ->  Fmt.(failwith "lookup_type: id %s refers to a module_type or module" h)

let rec lookup_module env = function
    REL h -> begin match Env.lookup env h with
        Env.ModuleType _ -> Fmt.(failwithf "lookup_module: path %s refers to a module_type" h)
      | Env.Type -> Fmt.(failwithf "lookup_module: internal error, path was %s" h)
      | Env.Module mty -> mty
    end
  | TOP _ as p -> Fmt.(failwithf "TOP should never appear in a module_path here: %a" pp_module_path_t p)
  | DEREF (p, id) -> begin match lookup_module env p with
        MtSig l -> begin match l |> List.find_map (function
            SiModuleBinding(h', mty) when id=h' -> Some mty
          | _ -> None) with
          None -> Fmt.(failwith "lookup_module: cannot resolve, env lookup failure" pp_module_path_t p)
        | Some mty -> mty
        end
    end
    

let lookup_module_type env p =
  match p with
    (None, h) -> begin match Env.lookup env h with
        Env.ModuleType mty -> mty
      | Env.Type | Env.Module _ -> Fmt.(failwithf "lookup_module_type: path %a did not map to module_type" string h)
    end
  | (Some mp, h) ->
    match lookup_module env mp with
      MtSig l -> begin
        match l |> List.find_map (function
              SiModuleType(h', mty) when h=h' -> Some mty
            | _ -> None) with
          None -> Fmt.(failwithf "lookup_module_type: cannot resolve, env lookup failure: %a.%s" pp_module_path_t mp h)
        | Some mty ->
          mty
      end

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

let rec tc_utype env ut =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt = function
      Ref (None, t) as ut ->
      let _ = lookup_type env t in
      ut

    | Ref (Some mpath, t) as ut -> begin match lookup_module env mpath with
          MtSig l ->
          if List.mem (SiType t) l then
            ut
          else 
            Fmt.(failwithf "tc_utype: utype %a not found in environment" pp_utype_t ut)
        | _ -> Fmt.(failwithf "tc_utype: module-path %a did not yield a signature" pp_module_path_t mpath)
      end
    | ut -> old_migrate_utype_t dt ut in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  dt.migrate_utype_t dt ut

and tc_struct_item env = function
    StTypes (recflag,l) ->
    let subenv = if recflag then
        l |> List.fold_left (fun env (id, _) -> Env.push_type env id)
          env
    else env in
    let l =
      l |> List.map (fun (id, ut) -> (id, tc_utype subenv ut)) in
    let newenv = List.fold_left (fun env (id, _) -> Env.push_type env id) env l in
    (newenv, l |> List.map (fun (id, _) -> SiType id))

  | StModuleBinding (mid, me) ->
    let mty = tc_module_expr env me in
    let si = SiModuleBinding(mid, mty) in
    (Env.push_module env (mid, mty), [si])

  | StImport(fname, mid) ->
    let stl = Utconv.load_file fname in
    let st = StModuleBinding (mid, MeStruct stl) in
    tc_struct_item env st

  | StLocal (l1, l2) ->
    let (env',_) = tc_structure env l1 in
    let (_, l2) = tc_structure env' l2 in
    tc_signature env l2

  | StOpen p as st ->
    let mty = lookup_module env p in begin match mty with
        MtSig l ->
        let (env, _) = tc_signature env l in
        (env, [])
        
      | _ -> Fmt.(failwith "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StInclude p as st ->
    let mty = lookup_module env p in begin match mty with
        MtSig l ->
        tc_signature env l

      | _ -> Fmt.(failwith "tc_struct_item: cannot typecheck %a, path does not denote a module" pp_struct_item_t st)
    end

  | StModuleType (id, mty) ->
    let mty = tc_module_type env mty in
    (Env.push_module_type env (id, mty), [SiModuleType (id, mty)])

and tc_structure env l =
  let (env, acc) = List.fold_left (fun (env,acc) st ->
      let (env', sil) = tc_struct_item env st in
      (env', sil::acc))
      (env, []) l in
  (env, List.concat (List.rev acc))

and tc_signature env l =
  let (env, acc) = List.fold_left (fun (env,acc) si ->
      let (env', sil) = tc_sig_item env si in
      (env', sil::acc))
    (env,[]) l in
  (env, List.concat (List.rev acc))
  
and tc_sig_item env = function
    SiType s as si -> (Env.push_type env s, [si])
  | SiModuleBinding (mid, mty) ->
    let mty = tc_module_type env mty in
    (Env.push_module env (mid, mty), [SiModuleBinding (mid, mty)])
  | SiModuleType (mid, mty) ->
    let mty = tc_module_type env mty in
    (Env.push_module_type env (mid, mty), [SiModuleType (mid, mty)])
  | SiInclude p ->
    let mty = lookup_module env p in begin match mty with
        MtSig l ->
        tc_signature env l
      | _ -> Fmt.(failwith "tc_sig_item: cannot typecheck %a, path does not denote a module" pp_module_path_t p)
    end


and tc_module_expr env = function
    MeStruct l ->
    l
    |> List.fold_left (fun (env,acc) st ->
        let (env', sil) = tc_struct_item env st in
        (env, sil@acc)) (env, [])
    |> (fun (_, acc) -> MtSig (List.rev acc))

  | MeFunctorApp(me1,me2) as me -> begin match (tc_module_expr env me1, tc_module_expr env me2) with
      (MtFunctorType((mid, formal_mty), resmty), actual_mty) ->
      let formal_mty = tc_module_type env formal_mty in
      let actual_mty = tc_module_type env actual_mty in
      let resmty = tc_module_type (Env.push_module env (mid, formal_mty)) resmty in
      satisfies_constraint env ~lhs:actual_mty ~rhs:formal_mty ;
      resmty

      | (lhs, _) -> Fmt.(failwithf "tc_module_expr: type of lhs not a functor-type %a" pp_module_type_t lhs)
    end

  | MePath p as st ->
    lookup_module env p

  | MeFunctor((mid, argmty), me) ->
    let argmty = tc_module_type env argmty in
    let resmty = tc_module_expr (Env.push_module env (mid, argmty)) me in
    MtFunctorType((mid, argmty), resmty)

and tc_module_type env = function
    MtSig l ->
    let (_, l) = tc_signature env l in
    MtSig l
  | MtFunctorType ((mid, argty), resty) ->
    let argty = tc_module_type env argty in
    let resty = tc_module_type (Env.push_module env (mid, argty)) resty in
    MtFunctorType ((mid, argty), resty)
  | MtPath (popt, id) ->
    lookup_module_type env (popt,id)

