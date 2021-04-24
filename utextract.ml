open Asttools
open Ututil
open Utypes
open Utio
open Utmigrate
open Uttypecheck

(* Plan of attack:

(0) general step: remove all vacuous StLocal


(1) convert "import" into a module-binding for the contents of the
    file being imported.  Check that it is closed.

    the empty first part of the local will tell us we can erase it.

    The next phase will add a type-constraint to the module,
    so we can later use it to know which names in <st2> to rewrite
    to point at <Fresh1>.

(2) convert "local <st1> in <st2> end" to
    "local in module <Fresh1> = struct <st1> end ;
     include <Fresh1> ;
     <st2> ;
     end ;"

    the empty first part of the local will tell us we can erase it.

    The next phase will add a type-constraint to the module,
    so we can later use it to know which names in <st2> to rewrite
    to point at <Fresh1>.

THEN run step #0

(3) assign fresh names to all unnamed functor-app-subterms.

(4) typecheck, which will also remove
   "import <uri> as M" in favor of
   "module M = struct <contents of uri> end"
   also inlines all module-type-references in other module-types

(5) remove "include" items, copying name-by-name; typecheck
    helpfully adorns "include" items with a signature, so
    we can just use that to guide our copying.

(6) run step #0

At this point, structs are:

  (a) types
  (b) module-bindings of module-exprs
  (c) open
  (d) module-types

module-exprs are:

  (a) structs
  (b) functors
  (c) functor-applications to named/cast args

(7) in all structures, replace instances of repeated names by:

  (a) first name is given a fresh name, and in sig-items between
      it and second name (inclusive) the fresh name is substituted
      for the old first name.

      Between two definitions of the same name, there might be an open;
      that open might define that name too.  That counts as the second
      definition, just as a type/module-binding would.

      But if an open is the first definition, then we don't need to
      rename it (obvs).

(8) replace all relative module/type-references except those bound in
    functor-bodies by absolute module/type-references.

(9) eliminate cast-cast, viz. ( <me> : <mt> ) : <mt2>

(10) beta-reduce all functor-applications whose result is of type a sig...end.
    Repeat until no more exist.  Since each functor-argument is already a module-name,
    and the functor-application is the RHS of a module-binding, no new module-binding
    needs to be created.

    The only relative references that need replacing, are those in the body of the
    functor (now the struct produced by reduction, that refer to functor-formal-args.

(11) remove all module-types, functors, and un-contracted functors.

*)

(** remove local <structure1> in <structure2> end in schema.  Replace with

    module <Fresh> = struct <structure1> end ;
    <strucure2> (where references to names in <structure1> are replaced
    with references to those names with <Fresh> inserted.

*)

(** Typecheck and expand import <uri> as M to module M = struct <contents of uri> end *)

module Util = struct

let all_tids stl =
  let dt = make_dt () in
  let tids = ref [] in
  let add_tid tid = tids := tid:: !tids in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let old_migrate_sig_item_t = dt.migrate_sig_item_t in
  let new_migrate_utype_t dt ut = match ut with
      Ref(_, tid) -> add_tid tid ; ut
    | _ -> old_migrate_utype_t dt ut in
  let new_migrate_struct_item_t dt st = match st with
      StTypes (_, l) -> l |> List.iter (fun (tid, _, _) ->  add_tid tid) ;
      old_migrate_struct_item_t dt st
    | _ -> old_migrate_struct_item_t dt st in
  let new_migrate_sig_item_t dt si = match si with
      SiType (tid, _) -> add_tid tid ; si
    | _ -> old_migrate_sig_item_t dt si in
  let dt = { dt with
             migrate_utype_t = new_migrate_utype_t ;
             migrate_struct_item_t = new_migrate_struct_item_t ;
             migrate_sig_item_t = new_migrate_sig_item_t } in
  ignore (dt.migrate_structure dt stl) ;
  ID.sort_uniq_list !tids

let all_mids stl =
  let dt = make_dt () in
  let mids = ref [] in
  let add_mid mid = mids := mid:: !mids in
  let old_migrate_module_path_t = dt.migrate_module_path_t in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let old_migrate_sig_item_t = dt.migrate_sig_item_t in
  let old_migrate_module_expr_t = dt.migrate_module_expr_t in
  let old_migrate_module_type_t = dt.migrate_module_type_t in
  let new_migrate_module_path_t dt mp = match mp with
      REL mid -> add_mid mid ; mp
    | TOP mid -> add_mid mid ; mp
    | DEREF(mp, mid) -> add_mid mid ;
      old_migrate_module_path_t dt mp in
  let new_migrate_struct_item_t dt st = match st with
      StModuleBinding (mid, _) -> add_mid mid ;
      old_migrate_struct_item_t dt st
    | StImport (_, mid) -> add_mid mid ;
      old_migrate_struct_item_t dt st
    | StModuleType (mid, _) ->  add_mid mid ;
      old_migrate_struct_item_t dt st
    | _ -> old_migrate_struct_item_t dt st in
  let new_migrate_sig_item_t dt si = match si with
      SiModuleBinding (mid, _) -> add_mid mid ;
      old_migrate_sig_item_t dt si
    | SiModuleType (mid, _) -> add_mid mid ;
      old_migrate_sig_item_t dt si
    | _ -> old_migrate_sig_item_t dt si in
  let new_migrate_module_expr_t dt me = match me with
      MeFunctor ((mid, _), _) -> add_mid mid ;
      old_migrate_module_expr_t dt me
    | _ -> old_migrate_module_expr_t dt me in
  let new_migrate_module_type_t dt mt = match mt with
      MtFunctorType ((mid, _), _) -> add_mid mid ;
      old_migrate_module_type_t dt mt
    | _ -> old_migrate_module_type_t dt mt in
  let dt = { dt with
             migrate_module_path_t = new_migrate_module_path_t ;
             migrate_struct_item_t = new_migrate_struct_item_t ;
             migrate_sig_item_t = new_migrate_sig_item_t ;
             migrate_module_expr_t = new_migrate_module_expr_t ;
             migrate_module_type_t = new_migrate_module_type_t } in
  ignore (dt.migrate_structure dt stl) ;
  ID.sort_uniq_list !mids

module Fresh = struct
  type t = ID.t list ref
  let mk stl = ref (ID.sort_uniq_list ((all_mids stl)@(all_tids stl)))
  let fresh t s =
      let s' = ID.fresh !t s in
      t := s' :: !t ;
      s'
end

end

module ElimEmptyLocal = struct
  let exec stl =
    let dt = make_dt () in
    let old_migrate_structure = dt.migrate_structure in
    let new_migrate_structure dt stl =
      let stl = old_migrate_structure dt stl in
      stl |> List.concat_map (function
            StLocal([], l) -> l
          | st -> [st]) in
    let dt = { dt with migrate_structure = new_migrate_structure } in
    dt.migrate_structure dt stl
end

module ElimCastCast = struct
  let exec stl =
    let dt = make_dt () in
    let old_migrate_module_expr_t = dt.migrate_module_expr_t in
    let new_migrate_module_expr_t dt me = match old_migrate_module_expr_t dt me with
        MeCast(MeCast(me, _), mt) -> MeCast(me, mt)
      | me -> me in
    let dt = { dt with migrate_module_expr_t = new_migrate_module_expr_t } in
    dt.migrate_structure dt stl
end

module S1ElimImport = struct
  open Util
  let exec stl =
    let dt = make_dt () in
    let old_migrate_struct_item_t = dt.migrate_struct_item_t in
    let new_migrate_struct_item_t dt st = match st with
      | StImport(fname, mid) ->
        let stl = load_file fname in
        let st = StModuleBinding (mid, MeStruct stl) in
        old_migrate_struct_item_t dt st
      | _ -> old_migrate_struct_item_t dt st in
    let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
    dt.migrate_structure dt stl
end

module S2ElimLocal = struct
  open Util
  let exec stl =
    let mids = Fresh.mk stl in
    let dt = make_dt () in
    let old_migrate_struct_item_t = dt.migrate_struct_item_t in
    let new_migrate_struct_item_t dt st = match st with
        StLocal ([], _) -> st
      | StLocal ((_::_ as stl1), stl2) ->
        let mid = Fresh.fresh mids (ID.of_string "LOCAL") in
        let st = StLocal([],
                         [StModuleBinding(mid, MeStruct stl1);
                          StInclude(REL mid, None)]
                         @stl2) in
        old_migrate_struct_item_t dt st
      | _ -> old_migrate_struct_item_t dt st in
    let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
    dt.migrate_structure dt stl
end

module S3NameFunctorAppSubterms = struct
  open Util

  let name_functor_args mids me =
    let rec namerec acc me = match me with
        MeStruct _ -> (acc,me)
      | MeFunctorApp(me1, me2) ->
        let (acc, me1) = termrec acc me1 in
        let (acc, me2) = termrec acc me2 in
        (acc, MeFunctorApp(me1, me2))
      | MePath _ -> (acc, me)
      | MeFunctor _ -> (acc, me)
      | MeCast (me, mt) ->
        let (acc, me) = namerec acc me in
        (acc, MeCast (me, mt))
    and termrec acc me = match me with
        (MeStruct _
        | MeFunctor _) ->
        let mid = Fresh.fresh mids (ID.of_string "NAMED") in
        let acc = (StModuleBinding(mid, me))::acc in
        (acc, MePath(REL mid))
      | MeFunctorApp (me1, me2) ->
        let (acc, me1) = termrec acc me1 in
        let (acc, me2) = termrec acc me2 in
        (acc, MeFunctorApp (me1, me2))

      | MePath _ -> (acc, me)
      | MeCast (me, mt) ->
        let (acc, me) = termrec acc me in
        (acc, MeCast (me, mt)) in

    let (acc, me) = namerec [] me in
    (List.rev acc, me)

  let exec stl =
    let mids = Fresh.mk stl in
    let dt = make_dt () in
    let old_migrate_struct_item_t = dt.migrate_struct_item_t in
    let new_migrate_struct_item_t dt st = match st with
      StModuleBinding(mid, me) ->
      let me = dt.migrate_module_expr_t dt me in
      let (new_stl, new_me) = name_functor_args mids me in
      if [] = new_stl then
        StModuleBinding(mid, new_me)
      else
        StLocal([], new_stl@[StModuleBinding(mid, new_me)])
      | _ -> old_migrate_struct_item_t dt st in
    let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
    dt.migrate_structure dt stl

end

module S4Typecheck = struct

let exec stl = 
  let (_, (stl, _)) = tc_structure TEnv.mt stl in
  stl

end

(** remove "include <modulepath>" and replace with
    struct-items copying entry-by-entry. *)

module S5ElimInclude = struct

let exec stl =
  let dt = make_dt () in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let new_migrate_struct_item_t dt st = match st with
      StInclude (mp, None) ->
      Fmt.(failwithf "S3ElimInclude.exec: found unadorned StInclude (was typecheck already run?): %a"
             pp_struct_item_t st)
    | StInclude (mp, Some (MtSig l as mty)) ->
      let stl = l |> List.map (function
            SiType (tid, sealed) ->
            StTypes(false, [(tid, sealed, Ref(Some mp, tid))])
          | SiModuleBinding(mid, mty) ->
            StModuleBinding(mid, MeCast(MePath(DEREF(mp, mid)), mty))
          | SiModuleType(mid, mty) ->
            StModuleType(mid, mty)
          | SiInclude _ ->
            Fmt.(failwithf "S3ElimInclude.exec: include with malformed module-type: %a" pp_struct_item_t st)) in
      StLocal([], stl)
    | StInclude (mp, Some _) ->
      Fmt.(failwithf "S3ElimInclude.exec: internal error with StInclude: %a"
             pp_struct_item_t st)
    | _ -> old_migrate_struct_item_t dt st in
  let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
  dt.migrate_structure dt stl

end

(** rename names that are overridden later in the same structure
    Since these names are overridden, they cannot be seen outside
    the structure.  This means that this is a purely local
    modification.
 *)

module S7RenameOverridden = struct
open Util
module Env = Utypes.Env

module UE = struct
  type t = { uses : (unit, unit, unit) Env.t ; exports : (unit, unit, unit) Env.t }
  let mk ?(uses=Env.mk()) ?(exports=Env.mk()) () = {
    uses ; exports
  }

  let override_right ue1 ue2 =
    { uses = Env.merge ue1.uses (Env.sub ue2.uses ue1.exports)
    ; exports = Env.merge ue1.exports ue2.exports }

  let clear_exports {uses; exports} = { uses ; exports = Env.mk() }

  let merge ue1 ue2 = {
    uses = Env.merge ue1.uses ue2.uses
  ; exports = Env.merge ue1.exports ue2.exports
  }
end

module UseExport = UE

let utype_uses ut =
  let dt = make_dt () in
  let names = ref (Env.mk()) in
  let add_tid tid = names := Env.add_t !names (tid,()) in
  let add_mid mid = names := Env.add_m !names (mid,()) in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let old_migrate_module_path_t = dt.migrate_module_path_t in
  let new_migrate_utype_t dt ut = match ut with
      Ref (None, tid) -> add_tid tid ; ut
    | _ -> old_migrate_utype_t dt ut in
  let new_migrate_module_path_t dt mp = match mp with
    REL mid -> add_mid mid ; mp
    | _ -> old_migrate_module_path_t dt mp in
  let dt = { dt with
             migrate_utype_t = new_migrate_utype_t
           ; migrate_module_path_t = new_migrate_module_path_t } in
  ignore (dt.migrate_utype_t dt ut) ;
  Env.sort_uniq !names

let rec me_uses me = match me with
    MeStruct stl ->
    let ue = List.fold_left (fun ue st ->
        let st_ue = st_uses_exports st in
        UE.override_right ue st_ue
      ) (UE.mk()) stl in
    ue.UE.uses

  | MeFunctorApp (me1, me2) ->
    let uses_me1 = me_uses me1 in
    let uses_me2 = me_uses me2 in
    Env.merge uses_me1 uses_me2

  | MePath mp -> mp_uses mp

  | MeFunctor ((mid, argmty), me) ->
    assert FMV.(closed module_type argmty) ;
    let uses_me = me_uses me in
    Env.sub uses_me (Env.mk ~m:[mid,()] ())

  | MeCast(me, mty) ->
    assert FMV.(closed module_type mty) ;
    me_uses me

and mp_uses = function
    REL mid -> Env.mk ~m:[mid,()] ()
  | TOP _ as mp -> Env.mk ()
  | DEREF(mp, _) -> mp_uses mp

and st_uses_exports st = match st with
    StTypes (recflag, l) ->
    let uses =
      l
      |> List.map (fun (_, _, ut) -> utype_uses ut)
      |> List.fold_left Env.merge (Env.mk()) in
    let uses = if recflag then
        Env.sub uses (Env.mk ~t:(List.map (fun (id, _, _) -> (id, ())) l) ())
      else uses in
    UE.mk ~uses ~exports:(Env.mk ~t:(List.map (fun (id, _, _) -> (id, ())) l) ()) ()

  | StModuleBinding (mid, me) ->
    let uses = me_uses me in
    UE.mk ~uses ~exports:(Env.mk ~m:[mid,()] ()) ()

  | StModuleType (mid, mty) ->
    assert FMV.(closed module_type mty) ;
    UE.mk ~exports:(Env.mk ~m:[mid,()] ()) ()

  | StOpen (mp, Some (MtSig sil as mty)) ->
    assert FMV.(closed module_type mty) ;
    let uses = mp_uses mp in
    let exports = sil_exports sil in
    UE.mk ~uses ~exports ()

  | (StImport _
    | StLocal _
    | StInclude _
    | StOpen _
    ) ->
    Fmt.(failwithf "S7RenameOverridden.st_uses_exports: struct-item should have been eliminated: %s"
           (struct_item_to_string st))

and sil_exports sil =
  List.fold_left (fun n -> function
        SiType (tid, _) -> Env.add_t n (tid,())
      | SiModuleBinding (mid, mty) ->
        assert FMV.(closed module_type mty) ;
        Env.add_m n (mid,())

  | SiModuleType (mid, mty) ->
    assert FMV.(closed module_type mty) ;
    Env.add_m n (mid,())
  | SiInclude _ as si ->
    Fmt.(failwithf "sil_exports: internal error, unexpected %a" pp_sig_item_t si)
    ) (Env.mk ()) sil

let stl_uses_exports stl =
  List.map (fun st -> (st, st_uses_exports st)) stl

let stl_compute_accum stl_ue =
  List.fold_right (fun (st, ue) (l, cum_e) ->
      let new_cum_e = Env.merge cum_e ue.UE.exports in
      (((st, ue),cum_e)::l, new_cum_e)
    ) stl_ue ([], Env.mk ())

(** plan:

    Each entry in stl_ue_accum has:
    (1) the struct_item
    (2) its UE (uses, exports) names
    (3) the accumulated exported names of all struct_items
        after it.  Call this the "accumulated tail exports"
        (ATE).

    (4) If this entry exports a name, and the ATE has this name also,
    then we need to rename it.  If the entry is a recursive definition,
    then we need to take care during rename.

    This does not hold for "open"s, since they don't export names that
    can be seen outside the following struct-items.

    (5) If this entry *uses* a name, then there should be a renaming passed in
    in the env, which we will apply.

    This does apply to "open"s, since they use a module-name.

    By this point, all module-types are closed, so in step #4 we can just
    rename the definition and not bother passing forward in an env.

    This also means that there should be no uses of module-type-names in #5.

*)

let rename_ut env ut =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt ut = match ut with
      Ref (None, tid) -> begin match Env.lookup_t env tid with
          Some tid' -> Ref(None, tid')
        | None -> ut
      end
    | _ -> old_migrate_utype_t dt ut in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  dt.migrate_utype_t dt ut

let rename_mp env mp =
  let dt = make_dt () in
  let old_migrate_module_path_t = dt.migrate_module_path_t in
  let new_migrate_module_path_t dt mp = match mp with
      REL mid -> begin match Env.(lookup_m env mid) with
          Some v -> REL v
        | None -> mp
      end
    | _ -> old_migrate_module_path_t dt mp in
  let dt = { dt with migrate_module_path_t = new_migrate_module_path_t } in
  dt.migrate_module_path_t dt mp

let rec rename_st env st = match st with
    StTypes(false, l) ->
    StTypes(false, l |> List.map (fun (id, sealed, ut) -> (id, sealed, rename_ut env ut)))
  | StTypes(true, l) ->
    let env = Env.(sub env (mk ~t:(List.map (fun (id, _, _) -> (id, ())) l) ())) in
    StTypes(true, l |> List.map (fun (id, sealed, ut) -> (id, sealed, rename_ut env ut)))
  | StModuleBinding(mid, me) ->
    let env = Env.(sub env (mk ~m:[mid,()] ())) in
    StModuleBinding(mid, rename_me env me)
  | StModuleType (mid, mty) ->
    assert FMV.(closed module_type mty) ;
    st
  | StOpen (mp, Some mty) ->
    StOpen (rename_mp env mp, Some mty)

  | (StInclude _
    | StOpen (_, None)
    | StImport _
    | StLocal _
    ) -> Fmt.(failwithf "rename_st: forbidden struct_item: %a" pp_struct_item_t st)

and rename_me env me = match me with
    MeStruct l -> MeStruct (rename_structure env l)
  | MeFunctorApp (me1, me2) -> MeFunctorApp (rename_me env me1, rename_me env me2)
  | MePath mp -> MePath (rename_mp env mp)
  | MeFunctor ((mid, mty), me) ->
    assert FMV.(closed module_type mty) ;
    let env' = Env.(sub env (mk ~m:[mid,()] ())) in
    MeFunctor ((mid, mty), rename_me env' me)
  | MeCast (me, mt) ->
    assert FMV.(closed module_type mt) ;
    MeCast (rename_me env me, mt)

and rename_structure env l =
  let (_, acc) = List.fold_left (fun (env, acc) st ->
      let st = rename_st env st in
      let exports = (st_uses_exports st).UE.exports in
      let env' = Env.(sub env exports) in
      (env', st::acc)
    ) (env, []) l in
  List.rev acc

let rebind_st allnames exports_and_overridden (env, st) = match st with
    StTypes(false, l) ->
    let (env, revacc) = List.fold_left (fun (env, revacc) (tid, sealed, ut) ->
        if Env.has_t exports_and_overridden tid then
          let tid' = Fresh.fresh allnames tid in
          (Env.add_t env (tid, tid'), (tid', sealed, ut)::revacc)
        else (env, (tid, sealed, ut)::revacc)
      ) (env, []) l in
    (env, StTypes(false, List.rev revacc))
  | StTypes(true, l) ->
    let torebind = Env.(intersect exports_and_overridden (mk ~t:(l |> List.map (fun (tid, _, _) -> (tid, ()))) ())) in
    let renv =
      torebind
      |> Env.dom_t
      |> List.map (fun tid -> (tid, Fresh.fresh allnames tid))
      |> (fun l -> Env.mk ~t:l ()) in
    let l = l |> List.map (fun (tid, sealed, ut) ->
        let tid = match Env.lookup_t renv tid with
            Some tid' -> tid'
          | None -> tid in
        let ut = rename_ut renv ut in
        (tid, sealed, ut)) in
    (Env.merge renv env, StTypes(true, l))

  | StModuleBinding(mid, me) ->
    assert Env.(has_m exports_and_overridden mid) ;
    let mid' = Fresh.fresh allnames mid in
    (Env.(add_m env (mid, mid')), StModuleBinding(mid', me))

  | StModuleType (_, mty) ->
    assert FMV.(closed module_type mty) ;
    (env, st)

  | StOpen (mp, Some _) -> (env, st)

  | (StInclude _
    | StOpen (_, None)
    | StImport _
    | StLocal _
    ) -> Fmt.(failwithf "rebind_st: forbidden struct_item: %a" pp_struct_item_t st)

let rename_members allnames stl_ue_accum =
  let rename1 (renenv, acc) ((st, ue), rhs_export_accum) =
    let uses_and_overridden = Env.(intersect ue.UE.uses rhs_export_accum) in
    let exports_and_overridden = Env.(intersect ue.UE.exports rhs_export_accum) in
    let st = if Env.nonempty uses_and_overridden then
        rename_st renenv st
      else st in
    let (renenv, st) = if Env.nonempty exports_and_overridden then
        rebind_st allnames exports_and_overridden (renenv, st)
      else (renenv, st) in
    (renenv, st::acc) in
  let (_, revacc) = List.fold_left rename1 (Env.mk(), []) stl_ue_accum in
  List.rev revacc

let exec stl =
  let fresh = Fresh.mk stl in   
  let dt = make_dt () in
  let old_migrate_structure = dt.migrate_structure in
  let new_migrate_structure dt stl =
    let stl = old_migrate_structure dt stl in
    let stl_ue = stl_uses_exports stl in
    let (stl_ue_cum, _) = stl_compute_accum stl_ue in
    let stl = rename_members fresh stl_ue_cum in
    stl in

  let dt = { dt with migrate_structure = new_migrate_structure } in
  dt.migrate_structure dt stl

end

module S8Absolute = struct

type abs_env_t = (utype_t, module_path_t, unit) Env.t
[@@deriving show { with_path = false },eq]

let abs_mp env mp =
  let dt = make_dt () in
  let old_migrate_module_path_t = dt.migrate_module_path_t in
  let new_migrate_module_path_t dt mp = match mp with
      REL mid -> begin match Env.(lookup_m env mid) with
          None -> Fmt.(failwithf "abs_mp: module name %a not found in env %a"
                         ID.pp mid pp_abs_env_t env)
        | Some me -> me
      end
    | _ -> old_migrate_module_path_t dt mp in
  let dt = { dt with migrate_module_path_t = new_migrate_module_path_t } in
  dt.migrate_module_path_t dt mp

let subst_ut env ut =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt ut = match ut with
      Ref(None, tid) -> begin match Env.(lookup_t env tid) with
          None -> ut
        | Some ut -> ut
      end
    | Ref(Some mp, tid) -> Ref(Some (abs_mp env mp), tid)

    | _ -> old_migrate_utype_t dt ut in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  dt.migrate_utype_t dt ut

let rec abs_me prefix env me = match me with
    MeStruct l ->
    let (_, l) = abs_stl prefix env l in
    MeStruct l

  | MeFunctorApp(me1, me2) ->
    MeFunctorApp(abs_me prefix env me1, abs_me prefix env me2)

  | MePath mp -> MePath(abs_mp env mp)

  | MeFunctor((mid, mty), me) ->
    let env = Env.(add_m env (mid, REL mid)) in
    MeFunctor((mid, mty), abs_me None env me)

  | MeCast(me, mt) ->
    MeCast(abs_me prefix env me, mt)

and abs_st prefix env st = match (prefix, st) with
    (Some mpopt, StTypes(false, l)) ->
    let l = l |> List.map (fun (tid, sealed, ut) ->
        (tid, sealed, subst_ut env ut)) in
    let env' = Env.mk ~t:(l |> List.map (fun (tid, _, _) -> (tid, Ref (mpopt, tid)))) () in 
    let env = Env.(merge env' env) in
    (env, StTypes(false, l))
  | (Some mpopt, StTypes(true, l)) ->
    let env' = Env.mk ~t:(l |> List.map (fun (tid, _, _) -> (tid, Ref (mpopt, tid)))) () in 
    let env = Env.(merge env' env) in
    let l = l |> List.map (fun (tid, sealed, ut) ->
        (tid, sealed, subst_ut env ut)) in
    (env, StTypes(false, l))

  | (None, StTypes(recflag, l)) ->
    let env' = Env.(sub_tids env (List.map (fun (x, _, _) -> x) l)) in
    let l = l |> List.map (fun (tid, sealed, ut) ->
        (tid, sealed, subst_ut env' ut)) in
    (env', StTypes(recflag, l))

  | (None, StModuleBinding(mid, me)) ->
    let me = abs_me None env me in
    let env = Env.(sub_mids env [mid]) in
    (env, StModuleBinding(mid, me))

  | (Some mpopt, StModuleBinding(mid, me)) ->
    let prefixmp = match mpopt with None -> TOP mid | Some mp -> DEREF(mp, mid) in
    let me = abs_me (Some (Some prefixmp)) env me in
    let env = Env.(add_m env (mid, prefixmp)) in
    (env, StModuleBinding(mid, me))

  | (_, StOpen (mp, Some (MtSig l))) ->
    let mp = abs_mp env mp in
    let env = List.fold_left (fun env -> function
          SiType (tid, _) ->
          Env.(add_t env (tid, Ref(Some mp, tid)))
        | SiModuleBinding(mid, _) ->
          Env.(add_m env (mid, DEREF(mp, mid)))
        | SiModuleType _ -> env
        | SiInclude _ as si -> 
          Fmt.(failwithf "S8Absolute.abs_st: forbidden sig_item %a" pp_sig_item_t si)
      ) env l in
    let st = StOpen (mp, Some (MtSig l)) in
    (env, st)

  | (_, StModuleType (_, _)) -> (env, st)
  | (_, (StOpen(_, None)
        | StImport _
        | StLocal _
        | StInclude _)) -> Fmt.(failwithf "S8Absolute.abs_st: forbidden struct_item %a" pp_struct_item_t st)

and abs_stl prefix env stl =
  let (env, revacc) = List.fold_left (fun (env, revacc) st ->
      let (env, st) = abs_st prefix env st in
      (env, st::revacc)
    ) (env, []) stl in
  (env, List.rev revacc)

let exec stl =
  let (_, stl) = abs_stl (Some None) (Env.mk()) stl in
  stl
end

module S10ReduceFunctorApp = struct

let rec find_functor = function
    ([], _) -> Fmt.(failwithf "find_functor: path was empty")
  | ([h], l) ->
    l |> List.find_map (function
          StModuleBinding (mid, (MeFunctor _ as me)) when mid = h -> Some me
        | StModuleBinding (mid, MeCast(MeFunctor _ as me, _)) when mid = h -> Some me
        | _ -> None)
  | (h::t, l) ->
    l |> List.find_map (function
          StModuleBinding (mid, MeStruct l) when mid = h -> find_functor (t, l)
        | StModuleBinding (mid, MeCast(MeStruct l, _)) when mid = h -> find_functor (t, l)
        | _ -> None)

let unpack_functor_app me =
  let rec unrec argacc = function
      MePath mp -> (mp, argacc)
    | MeCast(me, _) -> unrec argacc me
    | MeFunctorApp(me1, MePath mp) -> unrec (mp::argacc) me1
    | MeFunctorApp(me1, MeCast(me2, _)) ->
      unrec argacc (MeFunctorApp(me1, me2))
    | _ -> Fmt.(failwithf "unpack_functor_app: unexpected %a" pp_module_expr_t me) in
  unrec [] me

let mp_to_list mp =
  let rec mprec acc = function
    TOP mid -> mid::acc
  | REL _ -> Fmt.(failwithf "mp_to_list: Should not find relative module-path here: %a" pp_module_path_t mp)
  | DEREF(mp, mid) -> mprec (mid::acc) mp
  in mprec [] mp

let unpack_functor me =
  let rec unrec acc = function
      MeCast(me, _) -> unrec acc me
    | MeFunctor((mid, mty), me) ->
      unrec ((mid, mty)::acc) me
    | MeStruct l -> (List.rev acc, l)
    | _ -> Fmt.(failwithf "unpack_functor: unexpected module_expr %a" pp_module_expr_t me)
  in unrec [] me

let rec reduce_stl prefixmp top stl =
  List.map (reduce_st prefixmp top) stl

and reduce_me prefixmp top me = match me with
  | MeCast(MeFunctorApp _ as me, MtSig _) ->
    let (fmp, argmps) = unpack_functor_app me in
    let fexp = match find_functor (mp_to_list fmp, top) with
        None -> Fmt.(failwithf "reduce_me: could not find functor at module path %a" pp_module_path_t fmp)
      | Some e -> e in
    let (formals, stl) = unpack_functor fexp in
    if List.length argmps <> List.length formals then
      Fmt.(failwithf "formal/actual length mismatch in %a" pp_module_expr_t me) ;
    let env = Env.mk ~m:(List.map2 (fun (mid, mty) argmp -> (mid, argmp)) formals argmps) () in
    let (_, stl) = S8Absolute.abs_stl (Some prefixmp) env stl in
    MeStruct stl

  | MeCast(me, mt) ->
    MeCast(reduce_me prefixmp top me, mt)
  | MeStruct l -> MeStruct (List.map (reduce_st prefixmp top) l)
  | (MeFunctor _
    | MeFunctorApp _
    | MePath _) -> me

and reduce_st prefixmp top st = match st with
  | StModuleBinding (mid, me) ->
    let prefixmp = match prefixmp with None -> TOP mid | Some mp -> DEREF(mp, mid) in
    StModuleBinding (mid, reduce_me (Some prefixmp) top me)
  | st -> st

let rec fix f x =
  let x' = f x in
  if x = x' then x
    else fix f x'

let exec stl =
  fix (fun stl -> reduce_stl None stl stl) stl

end

module S11NukeFunctorsSigs = struct

let exec stl =
  let dt = make_dt () in
  let old_migrate_structure = dt.migrate_structure in
  let old_migrate_module_expr_t = dt.migrate_module_expr_t in
  let new_migrate_module_expr_t dt me = match me with
      MeCast(me, _) -> dt.migrate_module_expr_t dt me
    | _ -> old_migrate_module_expr_t dt me in

  let new_migrate_structure dt stl =
    let stl = old_migrate_structure dt stl in
    List.fold_right (fun st acc ->
        match st with
  | StModuleBinding(_, MeStruct _) -> st::acc
  | StTypes _ -> st::acc
  | _ -> acc
      ) stl [] in
  let dt = { dt with
             migrate_structure = new_migrate_structure
           ; migrate_module_expr_t = new_migrate_module_expr_t
           } in
  dt.migrate_structure dt stl
end

(** Final extraction: returns a list of:

(module_path_t option * ID.t) * utype_t

where the module_path_t, if present, is an absolute module-path.
*)

module FinalExtract = struct

let check_closed l =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let defined = List.map fst l in
  let new_migrate_utype_t dt ut = match ut with
      Ref (mpopt, id) ->
      if not (List.mem (mpopt, id) defined) then
        Fmt.(failwithf "FinalExtract.check_closed: reference %s not defined in extracted list" (Normal.printer ut)) ;
      ut
    | _ -> old_migrate_utype_t dt ut in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  l |> List.iter (fun (_, ut) -> ignore(dt.migrate_utype_t dt ut))

let rec extract_stl mpopt acc stl =
  List.fold_left (extract_st mpopt) acc stl

and extract_st mpopt acc = function
    StTypes(_, l) ->
    List.fold_left (fun acc (id, _, ut) -> ((mpopt, id), ut)::acc) acc l

  | StModuleBinding(mid, MeStruct l) ->
    let mpopt = match mpopt with None -> Some(TOP mid) | Some mp -> Some(DEREF(mp, mid)) in
    extract_stl mpopt acc l

  | st -> Fmt.(failwithf "FinalExtract: st should have been erased@.%s@." (Normal.struct_item_printer st))

let exec stl =
  let l = List.rev (extract_stl None [] stl) in
  check_closed l ;
  l

end


let full_extract x =
  x
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
  |> ElimEmptyLocal.exec
  |> S7RenameOverridden.exec
  |> S8Absolute.exec
  |> ElimCastCast.exec
  |> S10ReduceFunctorApp.exec
  |> S11NukeFunctorsSigs.exec
