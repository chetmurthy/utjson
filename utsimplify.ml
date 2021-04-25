open Asttools
open Ututil
open Utypes
open Utio
open Utmigrate
open Uttypecheck


(** Coalesce atomic blocks and simplify conjunctions

(1) [ "a": t1 ] && [ "b": t2 ] -> [ "a": t1; "b": t2 ]

(2) (t1 && t2) && t3 -> t1 && (t2 && t3)

(3) (true && t) -> t, (t && true) -> t

(4) (false && t) -> false, (t && false) -> false

(5) sort conjuncts so that false, true, base-types are always first in
   conjunctions.

*)

let rec fix f x =
  let x' = f x in
  if x = x' then x else fix f x'

module CoalesceAtomics = struct

let list_of_ands ut =
  let rec lrec = function
      And(_, a,b) -> (lrec a)@(lrec b)
    | ut -> [ut]
  in lrec ut

let and_of_list loc l =
  assert (l <> []) ;
  let (last, l) = sep_last l in
  List.fold_right (fun ut l -> And(loc, ut, l)) l last

let simplify1 ut = match ut with
    And(_, UtFalse loc, _) -> UtFalse loc
  | And(_, _, UtFalse loc) -> UtFalse loc
  | And(_, UtTrue _, ut) -> ut
  | And(_, ut, UtTrue _) -> ut
  | And(loc, Atomic (loc2, l1), And(_, Atomic (_, l2), ut)) -> And(loc, Atomic (loc2, l1@l2), ut)
  | And(_, Atomic (loc, l1), Atomic (_, l2)) -> Atomic (loc, l1@l2)
  | And(loc, And(_, ut1,ut2), ut3) -> And(loc, ut1, And(loc_of_utype ut2, ut2, ut3))
  | _ -> ut

let simplify ut = fix simplify1 ut

let exec stl =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt ut =
    let loc = loc_of_utype ut in
    let ut = ut |> list_of_ands |> List.stable_sort Reloc.(wrap_cmp Stdlib.compare utype_t) |> and_of_list loc in
    let ut = simplify ut in
    let ut = old_migrate_utype_t dt ut in
    simplify ut in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  dt.migrate_structure dt stl
end

(** conversion introduces lots of superfluous base types, viz.

    "tags": array && [ of string ] && [ unique ] && array && [ size
   [1,max] ];

We can nuke these by observing that in conjunctions, CoalesceAtomics
   sorts simple types to the left.  So as we walk down the and/or
   tree, we just remember the base-type we've seen (if any) and when
   we see another, it's either the same (in which case, rewrite it to
   "true" or different, in which case rewrite it to "false".

Then do another CoalesceAtomics pass.

*)

module S2SuppressSuperfluousBaseTypes = struct

let rec simplify1 bt_opt ut = match (bt_opt, ut) with
    (Some bt0, And(loc, Simple (_, bt1), ut)) ->
    if bt0 = bt1 then simplify1 bt_opt ut
    else UtFalse loc
  | (Some bt0, Simple (loc, bt1)) ->
    if bt0 = bt1 then Simple (loc, bt0)
    else UtFalse loc
  | (None, And(loc, Simple (loc2, bt), ut)) ->
    And(loc, Simple (loc2, bt), simplify1 (Some bt) ut)
  | (_, And(loc, ut1, ut2)) -> And(loc, simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Or(loc, ut1, ut2)) -> Or(loc, simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Xor(loc, ut1, ut2)) -> Xor(loc, simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Not (loc, ut)) -> Not(loc, simplify1 bt_opt ut)
  | _ -> ut

let simplify x = fix (simplify1 None) x

let exec stl =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt ut =
    ut
    |> old_migrate_utype_t dt
    |> simplify
  in
  let dt = { dt with migrate_utype_t = new_migrate_utype_t } in
  dt.migrate_structure dt stl

end

let full_simplify x = x
 |> CoalesceAtomics.exec
 |> S2SuppressSuperfluousBaseTypes.exec
