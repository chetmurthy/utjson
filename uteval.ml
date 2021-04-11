open Asttools
open Ututil
open Utypes
open Utmigrate
open Uttypecheck

(* Plan of attack:

(1) convert "local <st1> in <st2> end" to
    "local in module <Fresh1> = struct <st1> end ;
     include <Fresh1> ;
     <st2> ;
     end ;"

    the empty first part of the local will tell us we can erase it.

    The next phase will add a type-constraint to the module,
    so we can later use it to know which names in <st2> to rewrite
    to point at <Fresh1>.

(2) typecheck, which will also remove
   "import <uri> as M" in favor of
   "module M = struct <contents of uri> end"
   also inlines all module-type-references in other module-types

(3) remove "include" blocks, copying name-by-name

(5) map signature-constrained modules to a new module with just the
    entries that the signature lets thru.

At this point, structs are:

  (a) types
  (b) module-bindings of module-exprs
  (c) open
  (d) module-types

module-expressions are:

  (a) structs
  (b) functors
  (c) functor-applications

(6) in all structures, replace instances of repeated names by:

  (a) first name is given a fresh name, and in sig-items between
      it and second name (inclusive) te fresh name is substituted
      for the old first name.

(7) In all functor-applications, replace structs or functor-applications
    in argument-position with fresh-named module-building just prior to
    application.

(8) replace all relative module/type-references except those bound in
    functor-bodies by absolute module/type-references.

(8) beta-reduce all functor-applications whose result is of type a sig...end.
    Repeat until no more exist.  Since each functor-argument is already a module-name,
    and the functor-application is the RHS of a module-binding, no new module-binding
    needs to be created.

    The only relative references that need replacing, are those in the body of the
    functor (now the struct produced by reduction, that refer to functor-formal-args.

(9) remove all module-types, functors, and un-contracted functors.

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
    | TOP _ -> Fmt.(failwithf "S3UnLocal: internal error %a" pp_module_path_t mp)
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
  List.sort_uniq Stdlib.compare !mids

end

module S1ElimLocal = struct
  let exec stl =
    let mids = ref (Util.all_mids stl) in
    let fresh s =
      let s' = MID.fresh !mids (MID.of_string s) in
      mids := s' :: !mids ;
      s' in
    let dt = make_dt () in
    let old_migrate_struct_item_t = dt.migrate_struct_item_t in
    let new_migrate_struct_item_t dt st = match st with
        StLocal (stl1, stl2) ->
        let mid = fresh "LOCAL" in
        let st = StLocal([],
                         [StModuleBinding(mid, MeStruct stl1);
                          StInclude(REL mid, None)]
                         @stl2) in
        old_migrate_struct_item_t dt st
      | _ -> old_migrate_struct_item_t dt st in
    let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
    dt.migrate_structure dt stl

end


module S2Typecheck = struct

let exec stl = 
  let (_, (stl, _)) = tc_structure Env.mt stl in
  stl

end

(** remove "include <modulepath>" and replace with
    struct-items copying entry-by-entry. *)

module S3ElimInclude = struct

end

module Absolute = struct



end


module EvalFunctors = struct

end
