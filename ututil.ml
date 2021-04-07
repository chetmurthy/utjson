
(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let rec uniquize = function
    [] -> []
  | (h::t) -> if List.mem h t then uniquize t else h::(uniquize t)

let rec sep_last = function
    [] -> failwith "sep_last"
  | hd::[] -> (hd,[])
  | hd::tl ->
      let (l,tl) = sep_last tl in (l,hd::tl)

