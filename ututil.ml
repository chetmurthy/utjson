
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

let push l x = (l := x :: !l)

let diff_set l1 l2 =
  if l2 = [] then l1 else List.filter (fun x -> not (List.mem x l2)) l1
let subtract = diff_set
let intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1
