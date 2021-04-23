
(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

module Stack = struct
let push l x = (l := x :: !l)
let pop l =
    match !l with
    h::tl -> l := tl
  | [] -> invalid_arg "pop"

let top l = List.hd !l
let empty l = [] = !l
end

let mkdir_p s =
  let p = Fpath.v s in
  let rec mkrec p =
    if p |> Bos.OS.Dir.exists |> Rresult.R.get_ok then ()
    else begin
      mkrec (Fpath.parent p) ;
      ignore (Bos.OS.U.mkdir p 0o755)
    end
  in mkrec p

let rec traverse_json j p = match (j,p) with
    (j,[]) -> j
  | (`Assoc l, h::t) -> begin match List.assoc h l with
        v -> traverse_json v t
      | exception Not_found ->
        Fmt.(failwithf "traverse_json: path %a was not valid for JSON@.%s@."
               (list Dump.string) p
               (Yojson.Basic.pretty_to_string j))
    end
  | (`List l, h::t) -> begin match int_of_string h with
        n -> if n < List.length l then
          traverse_json (List.nth l n) t
        else Fmt.(failwith "traverse_json: path component %a was not an integer for JSON@.%s@."
                    Dump.string h
                    (Yojson.Basic.pretty_to_string j))
      | exception Not_found ->
        Fmt.(failwithf "traverse_json: path %a was not valid for JSON@.%s@."
               (list Dump.string) p
               (Yojson.Basic.pretty_to_string j))
    end
  | _ ->
    Fmt.(failwithf "traverse_json: path %a was not valid for JSON@.%s@."
           (list Dump.string) p
           (Yojson.Basic.pretty_to_string j))

let key_compare (k1, _) (k2, _) = Stdlib.compare k1 k2
let canon_json (y : Yojson.Basic.t) : Yojson.Basic.t =
  let rec yrec = function
    `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int n -> `Float (float_of_int n)
  | `String s -> `String s
  | `List l -> `List (List.map yrec l)
  | `Assoc l -> `Assoc (List.stable_sort key_compare (List.map (fun (k,v) -> (k,yrec v)) l))
  in yrec y
