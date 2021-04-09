
(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let rec uniquize = function
    [] -> []
  | (h::t) -> if List.mem h t then uniquize t else h::(uniquize t)

let sep_last = function
    [] -> failwith "sep_last"
  | l ->
    match List.rev l with
    h::t -> (h, List.rev t)
