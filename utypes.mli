module ID : sig
type t = {prefix: string;  index: int}
[@@deriving show { with_path = false },eq]

val of_string : string -> t
val to_string : t -> string
val fresh : t list -> t -> t
val sort_uniq_list : t list -> t list
end

type base_type_t =
  JNull | JString | JBool | JNumber | JArray | JObject
[@@deriving show { with_path = false },eq]

module Bound : sig
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

and module_path_t =
    REL of ID.t
  | TOP of ID.t
  | DEREF of module_path_t * ID.t

and utype_t =
    Simple of base_type_t
  | And of utype_t * utype_t
  | Or of utype_t * utype_t
  | Xor of utype_t * utype_t
  | Impl of utype_t * utype_t
  | Not of utype_t
  | Atomic of atomic_utype_t list
  | Ref of module_path_t option * ID.t
[@@deriving show { with_path = false },eq]

type struct_item_t =
    StTypes of bool * (ID.t * utype_t) list
  | StModuleBinding of ID.t * module_expr_t
  | StImport of string * ID.t
  | StLocal of structure * structure
  | StOpen of module_path_t * module_type_t option
  | StInclude of module_path_t * module_type_t option
  | StModuleType of ID.t * module_type_t

and structure = struct_item_t list

and module_expr_t =
    MeStruct of structure
  | MeFunctorApp of module_expr_t * module_expr_t
  | MePath of module_path_t
  | MeFunctor of (ID.t * module_type_t) * module_expr_t
  | MeCast of module_expr_t * module_type_t

and module_type_t =
    MtSig of signature
  | MtFunctorType of (ID.t * module_type_t) * module_type_t
  | MtPath of module_path_t option * ID.t

and signature = sig_item_t list

and sig_item_t =
    SiType of ID.t
  | SiModuleBinding of ID.t * module_type_t
  | SiModuleType of ID.t * module_type_t
  | SiInclude of module_path_t

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

val make_module_path : bool -> ID.t list -> module_path_t

module Env :
sig
  type ('a, 'b, 'c) t = { t : (ID.t * 'a) list; m : (ID.t * 'b) list; mt : (ID.t * 'c) list; }
[@@deriving show { with_path = false },eq]
  val mk :
    ?t:(ID.t * 'a) list -> ?m:(ID.t * 'b) list -> ?mt:(ID.t * 'c) list -> unit -> ('a, 'b, 'c) t
  val add_t : ('a, 'b, 'c) t -> (ID.t * 'a) -> ('a, 'b, 'c) t
  val add_m : ('a, 'b, 'c) t -> (ID.t * 'b) -> ('a, 'b, 'c) t
  val add_mt : ('a, 'b, 'c) t -> (ID.t * 'c) -> ('a, 'b, 'c) t
  val sort_uniq : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  val merge : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  val sub : ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t
  val lookup_t : ('a, 'b, 'c) t -> ID.t -> 'a option
  val lookup_m : ('a, 'b, 'c) t -> ID.t -> 'b option
  val lookup_mt : ('a, 'b, 'c) t -> ID.t -> 'c option
  val has_t : ('a, 'b, 'c) t -> ID.t -> bool
  val has_m : ('a, 'b, 'c) t -> ID.t -> bool
  val has_mt : ('a, 'b, 'c) t -> ID.t -> bool
  val empty : ('a, 'b, 'c) t -> bool
  val nonempty : ('a, 'b, 'c) t -> bool
  val intersect : ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t
  val dom_t : ('a, 'b, 'c) t -> ID.t list
  val dom_m : ('a, 'b, 'c) t -> ID.t list
  val dom_mt : ('a, 'b, 'c) t -> ID.t list
end
