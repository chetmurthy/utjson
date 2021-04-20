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
      And(a,b) -> (lrec a)@(lrec b)
    | ut -> [ut]
  in lrec ut

let and_of_list l =
  assert (l <> []) ;
  let (last, l) = sep_last l in
  List.fold_right (fun ut l -> And(ut, l)) l last

let simplify1 ut = match ut with
    And(UtFalse, _) -> UtFalse
  | And(_, UtFalse) -> UtFalse
  | And(UtTrue, ut) -> ut
  | And(ut, UtTrue) -> ut
  | And(Atomic l1, And(Atomic l2, ut)) -> And(Atomic (l1@l2), ut)
  | And(Atomic l1, Atomic l2) -> Atomic (l1@l2)
  | And(And(ut1,ut2), ut3) -> And(ut1, And(ut2, ut3))
  | _ -> ut

let simplify ut = fix simplify1 ut

let exec stl =
  let dt = make_dt () in
  let old_migrate_utype_t = dt.migrate_utype_t in
  let new_migrate_utype_t dt ut =
    let ut = ut |> list_of_ands |> List.stable_sort Stdlib.compare |> and_of_list in
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
    (Some bt0, And(Simple bt1, ut)) ->
    if bt0 = bt1 then simplify1 bt_opt ut
    else UtFalse
  | (Some bt0, Simple bt1) ->
    if bt0 = bt1 then Simple bt0
    else UtFalse
  | (None, And(Simple bt, ut)) ->
    And(Simple bt, simplify1 (Some bt) ut)
  | (_, And(ut1, ut2)) -> And(simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Or(ut1, ut2)) -> Or(simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Xor(ut1, ut2)) -> Xor(simplify1 bt_opt ut1, simplify1 bt_opt ut2)
  | (_, Not ut) -> Not(simplify1 bt_opt ut)
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
