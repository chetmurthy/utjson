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

  let counter = ref 0
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    mid

  let uri2type = ref []
  let add_uri2type s r = uri2type := (s,r) :: !uri2type

  let imports = ref []
  let add_imports s mid = imports := (s, mid):: !imports 

  let lookup_ref s =
    match List.assoc s !uri2type with
      r -> r
    | exception Not_found ->
      let mid = newmid() in
      let r = Ref([mid],"t") in
      add_imports s mid ;
      add_uri2type s r ;
      r

  let locals = ref [] 
  let forward_define_local s =
    add_uri2type (Printf.sprintf "#/definitions/%s" s) (Ref([], s)) ;
    ()
  let register_local_definition s t =
    locals := (s, t) :: !locals ;
    ()

  let reset () = 
    counter := 0 ;
    uri2type := [] ;
    imports := [] ;
    locals := []

  let conv_simple = function
      `String "null" -> [Simple JNull]
    | `String "string" -> [Simple JString]
    | `String "bool" -> [Simple JBool]
    | `String "number" -> [Simple JNumber]
    | `String "array" -> [Simple JArray]
    | `String "object" -> [Simple JObject]
    | `String "integer" -> [Ref (["Predefined"], "integer")]
    | v -> Fmt.(failwithf "conv_type: malformed type member: %a" pp_json v)

  let rec conv_type_l (j : json) = match j with
      `Assoc l ->
      (match List.assoc "definitions" l with
         `Assoc l ->
         l |> List.iter (fun (name, t) ->
             forward_define_local name ;
             let t = conv_type0 t in
             register_local_definition name t
           )

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
      List.fold_left (fun a b -> And(a,b)) (List.hd l) (List.tl l)
  
let conv_type t =
  reset () ;
  let t = conv_type0 t in
  let l = List.map (fun (id, mid) -> Import(id, mid)) !imports in
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
