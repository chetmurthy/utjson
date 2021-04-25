open Pa_ppx_base.Pp_MLast

open Ututil

[%%import: Utypes.loc]
[@@deriving show { with_path = false },eq]

module LJ = struct
[%%import: Utypes.LJ.t]
[@@deriving show { with_path = false },eq]

let to_json lj =
  let rec jrec = function
    | Null _ -> `Null
    | Bool (_, b) -> `Bool b
    | Int (_, n) -> `Int n
    | Float (_, f) -> `Float f
    | String (_, s) -> `String s
    | Assoc (_, l) -> `Assoc(l |> List.map (fun (k,j) -> (k,jrec j)))
    | List (_, l) -> `List(l |> List.map jrec)
  in jrec lj

let to_loc = function
    | Null loc
    | Bool (loc, _)
    | Int (loc, _)
    | Float (loc, _)
    | String (loc, _)
    | Assoc (loc, _)
    | List (loc, _) -> loc
end

module ID = struct
type t = [%import: Utypes.ID.t]
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
      if s=s' then Some m else None) in
  let maxnum = List.fold_left max n nums in
  {prefix=s; index=maxnum+1}

let sort_uniq_list l = List.sort_uniq Stdlib.compare l
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

let loc_of_utype = function
    UtTrue loc 
  | UtFalse loc
  | Simple (loc, _)
  | And (loc, _, _)
  | Or (loc, _, _)
  | Xor (loc, _, _)
  | Impl (loc, _, _)
  | Not (loc, _)
  | Atomic (loc, _)
  | Ref (loc, _)
  | Seal (loc, _, _, _) -> loc

let loc_of_struct_item = function
    StTypes (loc, _, _)
  | StModuleBinding (loc, _, _)
  | StImport (loc, _, _)
  | StLocal (loc, _, _)
  | StOpen (loc, _, _)
  | StInclude (loc, _, _)
  | StModuleType (loc, _, _) -> loc

let loc_of_structure = function
  h::t ->
  List.fold_left (fun loc st -> Ploc.encl loc (loc_of_struct_item st))
    (loc_of_struct_item h) t
  | [] -> Ploc.dummy

let loc_of_sig_item = function
    SiType (loc, _, _)
  | SiModuleBinding (loc, _, _)
  | SiModuleType (loc, _, _)
  | SiInclude (loc, _) -> loc

let loc_of_signature = function
  h::t ->
  List.fold_left (fun loc st -> Ploc.encl loc (loc_of_sig_item st))
    (loc_of_sig_item h) t
  | [] -> Ploc.dummy

let loc_of_module_type = function
    MtSig (loc, _)
  | MtFunctorType (loc, _, _)
  | MtPath (loc, _) -> loc

let loc_of_module_expr = function
    MeStruct (loc, _)
  | MeFunctorApp (loc, _, _)
  | MePath (loc, _)
  | MeFunctor (loc, _, _)
  | MeCast (loc, _, _) -> loc

let loc_of_atomic_utype = function
    Field (loc, _, _)
  | FieldRE (loc, _, _)
  | FieldRequired (loc, _)
  | ArrayOf (loc, _)
  | ArrayTuple (loc, _)
  | ArrayUnique loc
  | ArrayIndex (loc, _, _)
  | Size (loc, _)
  | StringRE (loc, _)
  | NumberBound (loc, _)
  | Sealed loc
  | OrElse (loc, _)
  | MultipleOf (loc, _)
  | Enum (loc, _)
  | Default (loc, _)
  | Format (loc, _)
  | PropertyNames (loc, _)
  | ContentMediaType (loc, _)
  | ContentEncoding (loc, _) -> loc

type token = [%import: Utypes.token]

let make_module_path abs l =
  match l with 
    [] -> Fmt.(failwithf "make_module_path: internal error, should never be called with []")
  | (h::t) ->
    List.fold_left (fun mp id -> DEREF(mp, id)) (if abs then TOP h else REL h) t

module Env = struct
  type ('a, 'b, 'c) t = { t : (ID.t * 'a) list; m : (ID.t * 'b) list; mt : (ID.t * 'c) list; }
  [@@deriving show { with_path = false },eq]
  let mk ?(t = []) ?(m=[]) ?(mt=[]) () = { t ; m ; mt }
  let add_t it newv = { it with t = newv:: it.t }
  let add_m it newv = { it with m = newv:: it.m }
  let add_mt it newv = { it with mt = newv:: it.mt }

  let sort_uniq {t; m; mt} = {
    t = ID.sort_uniq_list t
  ; m = ID.sort_uniq_list m
  ; mt = ID.sort_uniq_list mt
  }
  let merge n1 n2 = {
    t = (n1.t @ n2.t)
  ; m = (n1.m @ n2.m)
  ; mt = (n1.mt @ n2.mt)
  } |> sort_uniq

  let sub n1 n2 = {
    t = n1.t |> List.(filter (fun (id,_) -> not (mem_assoc id n2.t)))
  ; m = n1.m |> List.(filter (fun (id,_) -> not (mem_assoc id n2.m)))
  ; mt = n1.mt |> List.(filter (fun (id,_) -> not (mem_assoc id n2.mt)))
  }
  let sub_tids n l =
    sub n (mk ~t:(l |> List.map (fun id -> (id, ()))) ())
  let sub_mids n l =
    sub n (mk ~m:(l |> List.map (fun id -> (id, ()))) ())

  let lookup_t t k = List.assoc_opt k t.t
  let lookup_m t k = List.assoc_opt k t.m
  let lookup_mt t k = List.assoc_opt k t.mt
  let has_t t k = List.mem_assoc k t.t
  let has_m t k = List.mem_assoc k t.m
  let has_mt t k = List.mem_assoc k t.mt

  let merge n1 n2 = {
    t = (n1.t @ n2.t)
  ; m = (n1.m @ n2.m)
  ; mt = (n1.mt @ n2.mt)
  } |> sort_uniq

  let intersect n1 n2 = {
    t = n1.t |> List.(filter (fun (id,_) -> (mem_assoc id n2.t)))
  ; m = n1.m |> List.(filter (fun (id,_) -> (mem_assoc id n2.m)))
  ; mt = n1.mt |> List.(filter (fun (id,_) -> (mem_assoc id n2.mt)))
  }

  let empty = function
      { t=[]; m=[]; mt=[] } -> true
    | _ -> false
  let nonempty x = not (empty x)

  let dom_t {t; _} = List.map fst t
  let dom_m {m; _} = List.map fst m
  let dom_mt {mt; _} = List.map fst mt
end
