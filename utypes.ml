
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

type atomic_utype_t =
    Field of string * utype_t
  | FieldRE of string * utype_t
  | FieldRequired of string list
  | ArrayOf of utype_t
  | ArrayTuple of utype_t list
  | ArrayUnique
  | ArrayIndex of int * utype_t
  | Size of size_constraint_t
  | StringRE of string
  | NumberBound of range_constraint_t
  | Sealed of bool
  | OrElse of utype_t

and utype_t =
    Simple of base_type_t
  | And of utype_t * utype_t
  | Or of utype_t * utype_t
  | Not of utype_t
  | Atomic of atomic_utype_t list
  | Ref of string list * string
[@@deriving show { with_path = false },eq]

type struct_item_t =
    Decl of string * utype_t
  | Module of string * structure
  | Local of structure * structure

and structure = struct_item_t list
[@@deriving show { with_path = false },eq]

type token =
    Lident of string
  | Uident of string
  | Spcl of string
  | Keyw of string
  | String of string
  | Integer of string
  | Float of string
  | Regexp of string
  | EOF
