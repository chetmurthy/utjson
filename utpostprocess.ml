open Asttools
open Ututil
open Utypes
open Utio
open Utmigrate
open Uttypecheck

module UnfoldTop = struct

  let unfold tdl ut =
    let rec unrec stk ut = match ut with
        UtTrue _ -> ut
      | UtFalse _ -> ut
      | Simple _ -> ut
      | And (loc, ut1, ut2) -> And(loc, unrec stk ut1, unrec stk ut2)
      | Or (loc, ut1, ut2) -> Or(loc, unrec stk ut1, unrec stk ut2)
      | Xor(loc, ut1, ut2) -> Xor(loc, unrec stk ut1, unrec stk ut2)
      | Impl(loc, ut1, ut2) -> Impl(loc, unrec stk ut1, unrec stk ut2)
      | Not (loc, ut) -> Not(loc, unrec stk ut)
      | Atomic _ -> ut
      | Ref (loc, (mpopt, tid)) ->
    if List.mem (mpopt, tid) stk then ut
    else match List.assoc (mpopt, tid) tdl with
        v -> unrec ((mpopt, tid)::stk) v
      | exception Not_found ->
        Fmt.(failwith"UnfoldTop.unfold: reference %s not found" (Normal.printer ut))
    in unrec [] ut

  let exec tdl =
    tdl |> List.map (fun (n, ut) -> (n, unfold tdl ut))

end

let unfold_utype tdl uts =
  let ut = of_string_exn uts in
  UnfoldTop.unfold tdl ut
