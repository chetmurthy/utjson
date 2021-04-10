open Ututil

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
