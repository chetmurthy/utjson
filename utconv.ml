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

let conv_type t =
  let counter = ref 0 in
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    mid in

  let uri2type = ref [] in
  let add_uri2type s r = uri2type := (s,r) :: !uri2type in

  let imports = ref [] in
  let add_imports s mid = imports := (s, mid):: !imports  in

  let lookup_ref s =
    match List.assoc s !uri2type with
      r -> r
    | exception Not_found ->
      let mid = newmid() in
      let r = Ref([mid],"t") in
      add_imports s mid ;
      add_uri2type s r ;
      r in

  let locals = ref []  in
  let register_local s t =
    locals := (s, t) :: !locals ;
    add_uri2type (Printf.sprintf "#/definitions/%s" s) (Ref([], s)) ;
    () in

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
      (match List.assoc "definitions" l with
         `Assoc l ->
         l |> List.iter (fun (name, t) ->
             let t = conv_type0 t in
             register_local name t)

       | v -> Fmt.(failwithf "conv_type: malformed definitions member: %a" pp_json v)
       | exception Not_found -> ()
      ) ;
      (match List.assoc "type" l with
       | `String _ as j -> conv_simple j
       | exception Not_found ->
         (match List.assoc "$ref" l with
            `String s -> [lookup_ref s]
          | j -> Fmt.(failwithf "conv_type: $ref has malformed member: %a" pp_json j)
          | exception Not_found -> []
         )
      )@
      (match List.assoc "properties" l with
         `Assoc l ->
         [Atomic (List.map (fun (k,v) -> Field(k,conv_type0 v)) l)]
       | v -> Fmt.(failwithf "conv_type: malformed properties member: %a" pp_json v)
       | exception Not_found -> []
      )@
      (match List.assoc "patternProperties" l with
         `Assoc l ->
         [Atomic (List.map (fun (k,v) -> FieldRE("/"^k^"/",conv_type0 v)) l)]
       | v -> Fmt.(failwithf "conv_type: malformed patternProperties member: %a" pp_json v)
       | exception Not_found -> []
      )

    | j -> Fmt.(failwithf "conv_type: %a" pp_json j)

  and conv_type0 t =
    match conv_type_l t with
      [] -> Fmt.(failwithf "conv_type: conversion produced no result: %a" pp_json t)
    | l ->
      List.fold_left (fun a b -> And(a,b)) (List.hd l) (List.tl l) in
  
  let t = conv_type0 t in
  let l = List.map (fun (mid, id) -> Import(mid, id)) !imports in
  let l = if !locals = [] then l else
      l@[Decls(true, !locals)] in
  if l = [] then Decls(false, [("t", t)])
  else 
    Local(l, [Decls(false, [("t", t)])])

let load_file s =
  if Str.(string_match (regexp ".*\\.json$") s 0) then
    let j = Yojson.Basic.from_file s in
    conv_type j
  else if Str.(string_match (regexp ".*\\.utj$") s 0) then
    let l = Utparse0.(parse_file parse_utype_structure) s in
    Local([], l)
  else Fmt.(failwithf "load_file: format not recognized: %s" s)
