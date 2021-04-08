
type base_type_t =
  JNull | JString | JBool | JNumber | JArray | JObject
[@@deriving show { with_path = false },eq]

module Bound = struct
  type 'a t = { it : 'a ; exclusive : bool }
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
  | MultipleOf of float
  | Enum of Yojson.Basic.t list
  | Default of Yojson.Basic.t
  | Format of string
  | PropertyNames of utype_t
  | ContentMediaType of string
  | ContentEncoding of string

and utype_t =
    Simple of base_type_t
  | And of utype_t * utype_t
  | Or of utype_t * utype_t
  | Xor of utype_t * utype_t
  | Impl of utype_t * utype_t
  | Not of utype_t
  | Atomic of atomic_utype_t list
  | Ref of string list * string
[@@deriving show { with_path = false },eq]

type struct_item_t =
    Decls of bool * (string * utype_t) list
  | ModuleExpBinding of string * module_expr_t
  | Import of string * string
  | Local of structure * structure
  | Open of string list

and structure = struct_item_t list

and module_expr_t =
    Struct of structure
  | FunctorApp of module_expr_t * module_expr_t
  | ModuleName of string
  | Functor of (string * module_type_t) * module_expr_t

and module_type_t =
    Sig of signature
  | FunctorType of (string * module_type_t) * module_type_t

and signature = sig_item_t list

and sig_item_t =
    SimpleTypeBinding of string
  | ModuleTypeBinding of string * module_type_t

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
