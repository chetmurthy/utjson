
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

