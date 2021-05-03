open Pa_ppx_utils.Std

(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let raise_failwith loc s = Ploc.raise loc (Failure s)
let raise_failwithf loc fmt =
  Fmt.kstrf (raise_failwith loc) fmt

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

let canon l = 
  l
  |> List.sort_uniq Stdlib.compare
  |> List.stable_sort Stdlib.compare

let slice n m l =
  let alen = List.length l in
  let n = match n with
      None -> 0
    | Some n ->
      if n < 0 then
        if -n > alen then 0 else alen - n
      else
        if n > alen then alen else n in
  let m = match m with
      None -> alen
    | Some m ->
      if m < 0 then
        if -m > alen then alen else alen - m
      else
      if m > alen then alen else m in
  let l = nthtail l n in
  firstn (m-n) l
