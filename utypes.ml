
type base_type_t =
  JNull | JString | JBool | JNumber | JArray | JObject
[@@deriving show { with_path = false },eq]

module Bound = struct
  type 'a t = { it : 'a ; inclusive : bool }
  [@@deriving show { with_path = false },eq]
end

type size_constraint_t =
  (int Bound.t * int option Bound.t)
[@@deriving show { with_path = false },eq]

type range_constraint_t =
  (float option Bound.t * float option Bound.t)
[@@deriving show { with_path = false },eq]

type utype_t =
    And of utype_t * utype_t
  | Or of utype_t * utype_t
  | Not of utype_t
  | Simple of base_type_t
  | Field of string * utype_t
  | FieldRE of string * utype_t
  | FieldRequired of string
  | ArrayOf of utype_t
  | ArrayTuple of utype_t list
  | ArrayUnique
  | ArrayIndex of int * utype_t
  | Size of size_constraint_t
  | StringRE of string
  | NumberBound of range_constraint_t
  | Sealed of bool
  | OrElse of utype_t
  | Ref of string list
[@@deriving show { with_path = false },eq]

type decl_t = string * utype_t
[@@deriving show { with_path = false },eq]

type struct_item_t =
    Decl of decl_t
  | Module of structure
  | Local of structure * structure

and structure = struct_item_t list
[@@deriving show { with_path = false },eq]
