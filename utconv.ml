open Ututil
open Utypes

type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    ] [@@deriving show,eq]

type json_list = json list [@@deriving show,eq]

let conv_type j =
  let counter = ref 0 in
  let imports = ref [] in
  let register_import s =
    match List.assoc s !imports with
      mid -> Ref([mid],"t")
    | exception Not_found ->
      let mid = Printf.sprintf "M%d" !counter in
      let r = Ref([mid],"t") in
      counter := 1 + !counter ;
      imports := (s, mid):: !imports ;
      r in

  let conv_simple = function
      `String "null" -> [Simple JNull]
    | `String "string" -> [Simple JString]
    | `String "bool" -> [Simple JBool]
    | `String "number" -> [Simple JNumber]
    | `String "array" -> [Simple JArray]
    | `String "object" -> [Simple JObject]
    | `String "integer" -> [Ref (["Predefined"], "integer")]
    | v -> Fmt.(failwithf "conv_type: malformed type member: %a" pp_json v) in

  let rec conv_type_l (j : json) = match j with
      `Assoc l ->
      (match List.assoc "type" l with
       | `String _ as j -> conv_simple j
       | exception Not_found ->
         (match List.assoc "$ref" l with
            `String s -> [register_import s]
          | j -> Fmt.(failwithf "conv_type: $ref has malformed member: %a" pp_json j)
          | exception Not_found -> Fmt.(failwithf "conv_type: lacks type/$ref members: %a" pp_json j)
         )
      )@
      (match List.assoc "properties" l with
         `Assoc l ->
         [Atomic (List.map (fun (k,v) -> Field(k,conv_type0 v)) l)]
       | v -> Fmt.(failwithf "conv_type: malformed properties member: %a" pp_json v)
       | exception Not_found -> []
      )

    | j -> Fmt.(failwithf "conv_type: %a" pp_json j)

  and conv_type0 t =
    match conv_type_l t with
      [] -> Fmt.(failwithf "conv_type: conversion produced no result: %a" pp_json t)
    | l ->
      List.fold_left (fun a b -> And(a,b)) (List.hd l) (List.tl l)
  in
  let t = conv_type0 j in
  let imports = List.map (fun (mid, id) -> Import(mid, id)) !imports in
  Local(imports, [Decls(false, [("t", t)])])
