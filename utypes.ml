open Ututil

module MID = struct
type t = [%import: Utypes.MID.t]
[@@deriving show { with_path = false },eq]

let id_re = Str.regexp "^\\(.*[^0-9]\\)\\([0-9]*\\)$"
let of_string s =
  let open Str in
  if string_match id_re s 0 then
    let prefix = matched_group 1 s and
    num = matched_group 2 s in
    let num = if num = "" then -1 else int_of_string num in
    {prefix=prefix; index=num}
  else Fmt.(failwithf "fresh: internal error")

let to_string {prefix=s; index=n} =
  if -1 = n then s
  else Printf.(sprintf "%s%d" s n)

let fresh l {prefix=s;index=n} =
  let nums = l |> List.filter_map (fun {prefix=s';index=m} ->
      if s=s' then Some n else None) in
  let maxnum = List.fold_left max n nums in
  {prefix=s; index=maxnum+1}
end

type base_type_t = [%import: Utypes.base_type_t]
[@@deriving show { with_path = false },eq]

module Bound = struct
  type 'a t = [%import: 'a Utypes.Bound.t]
  [@@deriving show { with_path = false },eq]
end

type size_constraint_t = [%import: Utypes.size_constraint_t]
[@@deriving show { with_path = false },eq]

type range_constraint_t = [%import: Utypes.range_constraint_t]
[@@deriving show { with_path = false },eq]

[%%import: Utypes.atomic_utype_t]
[@@deriving show { with_path = false },eq]

[%%import: Utypes.struct_item_t]
[@@deriving show { with_path = false },eq]

type token = [%import: Utypes.token]

let make_module_path l =
  match l with 
    [] -> Fmt.(failwithf "make_module_path: internal error, should never be called with []")
  | (h::t) ->
    List.fold_left (fun mp id -> DEREF(mp, id)) (REL h) t
