module ID : sig
type t = {prefix: string;  index: int}
[@@deriving show { with_path = false },eq]

val of_string : string -> t
val to_string : t -> string
val fresh : t list -> t -> t
val sort_uniq_list : t list -> t list
end

type loc = Ploc.t
[@@deriving show { with_path = false },eq]

module LJ : sig
type t =
    | Null of loc
    | Bool of loc * bool
    | Int of loc * int
    | Float of loc * float
    | String of loc * string
    | Assoc of loc * (string * t) list
    | List of loc * t list
[@@deriving show { with_path = false },eq]
val to_json : t -> Yojson.Basic.t
val to_loc : t -> loc
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
    Field of loc * string * utype_t
  | FieldRequired of loc * string list
  | ArrayOf of loc * utype_t
  | ArrayTuple of loc * utype_t list
  | ArrayUnique of loc
  | ArrayIndex of loc * int * utype_t
  | Size of loc * size_constraint_t
  | StringRE of loc * string
  | NumberBound of loc * range_constraint_t
  | MultipleOf of loc * float
  | Enum of loc * Yojson.Basic.t list
  | Default of loc * Yojson.Basic.t
  | Format of loc * string
  | PropertyNames of loc * utype_t
  | ContentMediaType of loc * string
  | ContentEncoding of loc * string

and module_path_t =
    REL of ID.t
  | TOP of ID.t
  | DEREF of module_path_t * ID.t

and ref_t = module_path_t option * ID.t

and utype_t =
    UtTrue of loc
  | UtFalse of loc
  | Simple of loc * base_type_t
  | And of loc * utype_t * utype_t
  | Or of loc * utype_t * utype_t
  | Xor of loc * utype_t * utype_t
  | Impl of loc * utype_t * utype_t
  | Not of loc * utype_t
  | Atomic of loc * atomic_utype_t list
  | Ref of loc * ref_t
  | Seal of loc * utype_t * (string * utype_t) list * utype_t
[@@deriving show { with_path = false },eq]

module AN : sig
type t = SEALED | UNSEALED of base_type_t list
[@@deriving show { with_path = false }, eq]
type t_option = t option
[@@deriving show { with_path = false }, eq]
val mk : ?base_types:base_type_t list -> bool -> t
val sealed : t -> bool
end

type struct_item_t =
    StTypes of loc * bool * (ID.t * AN.t option * utype_t) list
  | StModuleBinding of loc * ID.t * module_expr_t
  | StImport of loc * string * ID.t
  | StLocal of loc * structure * structure
  | StOpen of loc * module_path_t * module_type_t option
  | StInclude of loc * module_path_t * module_type_t option
  | StModuleType of loc * ID.t * module_type_t

and structure = struct_item_t list

and module_expr_t =
    MeStruct of loc * structure
  | MeFunctorApp of loc * module_expr_t * module_expr_t
  | MePath of loc * module_path_t
  | MeFunctor of loc * (ID.t * module_type_t) * module_expr_t
  | MeCast of loc * module_expr_t * module_type_t

and module_type_t =
    MtSig of loc * signature
  | MtFunctorType of loc * (ID.t * module_type_t) * module_type_t
  | MtPath of loc * ref_t

and signature = sig_item_t list

and sig_item_t =
    SiType of loc * ID.t * AN.t
  | SiModuleBinding of loc * ID.t * module_type_t
  | SiModuleType of loc * ID.t * module_type_t
  | SiInclude of loc * module_path_t

and top_binding_t =
  loc * ref_t * AN.t * utype_t

and top_bindings = top_binding_t list
[@@deriving show { with_path = false },eq]


val loc_of_utype : utype_t -> loc
val loc_of_atomic_utype : atomic_utype_t -> loc
val loc_of_module_type : module_type_t -> loc
val loc_of_module_expr : module_expr_t -> loc
val loc_of_struct_item : struct_item_t -> loc
val loc_of_structure : structure -> loc
val loc_of_sig_item : sig_item_t -> loc
val loc_of_signature : signature -> loc

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
  val sub_tids : ('a, 'b, 'c) t -> ID.t list -> ('a, 'b, 'c) t
  val sub_mids : ('a, 'b, 'c) t -> ID.t list -> ('a, 'b, 'c) t
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
