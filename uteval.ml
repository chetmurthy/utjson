open Asttools
open Ututil
open Utypes
open Uttypecheck

(* Plan of attack:

(1) remove "import <uri> as M" in favor of module M = struct <contents of uri> end

(2) inline all module-type-references in other module-types

(3) remove local...in...end blocks in favor of freshly-named module

(4) remove "include" blocks, copying name-by-name

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

(** Expand import <uri> as M to module M = struct <contents of uri> end *)

module S1ExpandImport = struct

let exec stl =
  let dt = Utmigrate.make_dt () in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let new_migrate_struct_item_t dt = function
      StImport (fname, mid) ->
      let t = Utconv.load_file fname in
      let st = StModuleBinding (mid, MeStruct [t]) in
      old_migrate_struct_item_t dt st
    | st -> old_migrate_struct_item_t dt st in

  let dt = { dt with
             migrate_struct_item_t = new_migrate_struct_item_t } in
                 
  dt.migrate_structure dt stl

end

module UnLocal = struct

end

module Absolute = struct



end


module EvalFunctors = struct

end
