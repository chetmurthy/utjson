
(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let rec uniquize = function
    [] -> []
  | (h::t) -> if List.mem h t then uniquize t else h::(uniquize t)
