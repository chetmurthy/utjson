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

(9) beta-reduce all functor-applications whose result is of type a sig...end.
    Repeat until no more exist.  Since each functor-argument is already a module-name,
    and the functor-application is the RHS of a module-binding, no new module-binding
    needs to be created.

    The only relative references that need replacing, are those in the body of the
    functor (now the struct produced by reduction, that refer to functor-formal-args.

(10) remove all module-types, functors, and un-contracted functors.

*)

(** remove local <structure1> in <structure2> end in schema.  Replace with

    module <Fresh> = struct <structure1> end ;
    <strucure2> (where references to names in <structure1> are replaced
    with references to those names with <Fresh> inserted.

*)

(** Typecheck and expand import <uri> as M to module M = struct <contents of uri> end *)

module Util = struct

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
    | TOP _ -> Fmt.(failwithf "Util.all_mids: internal error %a" pp_module_path_t mp)
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
  let mk stl = ref (all_mids stl)
  let fresh t s =
      let s' = ID.fresh !t (ID.of_string s) in
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

module S1ElimImport = struct
  open Util
  let exec stl =
    let dt = make_dt () in
    let old_migrate_struct_item_t = dt.migrate_struct_item_t in
    let new_migrate_struct_item_t dt st = match st with
      | StImport(fname, mid) ->
        let stl = Utconv.load_file fname in
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
        let mid = Fresh.fresh mids "LOCAL" in
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
        let mid = Fresh.fresh mids "NAMED" in
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
            SiType tid ->
            StTypes(false, [(tid, Ref(Some mp, tid))])
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
  | TOP _ as mp -> Fmt.(failwithf "mp_uses: internal error %a" pp_module_path_t mp)
  | DEREF(mp, _) -> mp_uses mp

and st_uses_exports st = match st with
    StTypes (recflag, l) ->
    let uses =
      l
      |> List.map (fun (_, ut) -> utype_uses ut)
      |> List.fold_left Env.merge (Env.mk()) in
    let uses = if recflag then
        Env.sub uses (Env.mk ~t:(List.map (fun (id, _) -> (id, ())) l) ())
      else uses in
    UE.mk ~uses ~exports:(Env.mk ~t:(List.map (fun (id, _) -> (id, ())) l) ()) ()

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
        SiType tid -> Env.add_t n (tid,())
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

(** strategy:

    Each entry in stl_ue_accum has:
    (1) the struct_item
    (2) its UE (uses, exports) names
    (3) the accumulated exported names of all struct_items
        after it.  Call this the "accumulated tail exports"
        (ATE).

    If this entry exports a name, and the ATE has this name also,
    then we need to rename it.  If the entry is a recursive definition,
    then we need to take care during rename.

    If this entry *uses* a name, then there should be a renaming passed in
    in the env, which we will apply.

*)

let exec stl =
  let dt = make_dt () in
  let old_migrate_structure = dt.migrate_structure in
  let new_migrate_structure dt stl =
    let stl_ue = stl_uses_exports stl in
    let stl_ue_cum = stl_compute_accum stl_ue in

    stl in

  let dt = { dt with migrate_structure = new_migrate_structure } in
  dt.migrate_structure dt stl

end

module Absolute = struct



end


module EvalFunctors = struct

end
