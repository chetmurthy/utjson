open Asttools
open Ututil
open Utypes
open Utio
open Utmigrate
open Uttypecheck

module UnfoldTop = struct

  let unfold tdl ut =
    let rec unrec stk ut = match ut with
        UtTrue -> ut
      | UtFalse -> ut
      | Simple _ -> ut
  | And (ut1, ut2) -> And(unrec stk ut1, unrec stk ut2)
  | Or (ut1, ut2) -> Or(unrec stk ut1, unrec stk ut2)
  | Xor(ut1, ut2) -> Xor(unrec stk ut1, unrec stk ut2)
  | Impl(ut1, ut2) -> Impl(unrec stk ut1, unrec stk ut2)
  | Not ut -> Not(unrec stk ut)
  | Atomic _ -> ut
  | Ref (mpopt, tid) ->
    if List.mem (mpopt, tid) stk then ut
    else match List.assoc (mpopt, tid) tdl with
        v -> unrec ((mpopt, tid)::stk) v
      | exception Not_found ->
        Fmt.(failwith"UnfoldTop.unfold: reference %s not found" (Normal.printer ut))
    in unrec [] ut

  let exec tdl =
    tdl |> List.map (fun (n, ut) -> (n, unfold tdl ut))

end
