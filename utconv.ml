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

let isString (j : Yojson.Basic.t) = match j with
  `String _ -> true | _ -> false


let andList l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> And(a,b)) l last

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

  let assoc_opt k l =
    match List.assoc k l with
      v -> Some v
    | exception Not_found -> None

  let conv_simple = function
      `String "null" -> [Simple JNull]
    | `String "string" -> [Simple JString]
    | `String "boolean" -> [Simple JBool]
    | `String "number" -> [Simple JNumber]
    | `String "array" -> [Simple JArray]
    | `String "object" -> [Simple JObject]
    | `String "integer" -> [Ref (["Predefined"], "integer")]
    | `String "json" -> [Ref (["Predefined"], "json")]
    | `String "scalar" -> [Ref (["Predefined"], "scalar")]
    | v -> Fmt.(failwithf "conv_type: malformed type member: %a" pp_json v)

  let known_keys = [
    "$schema"; "$ref"; "$id"; "title"; "description";
    "type"; "properties"; "required"; "patternProperties" ;
    "exclusiveMinimum"; "exclusiveMaximum"; "minLength"; "maxLength";
    "items"; "additionalProperties"; "minItems"; "maxItems"; "uniqueItems";
    "id";
    "definitions";
    "enum"; "default";"pattern";"format";"propertyNames";
    "anyOf";"allOf";"oneOf";"not"
  ]

  let rec conv_type_l (j : json) = match j with
      `Assoc l ->
      let keys = List.map fst l in
      keys |> List.iter (fun k ->
          if not (List.mem k known_keys) then
            Fmt.(failwithf "conv_type: unrecognized object-key %s in %a" k pp_json j)
        ) ;
      (match assoc_opt "definitions" l with
         Some (`Assoc l) ->
         l |> List.iter (fun (name, _) ->
             forward_define_local name
           ) ;
         l |> List.iter (fun (name, t) ->
             let t = conv_type0 t in
             register_local_definition name t
           )

       | Some v -> Fmt.(failwithf "conv_type: malformed definitions member: %a" pp_json v)
       | None -> ()
      ) ;
      (match assoc_opt "type" l with
       | (Some (`String _ as j)) -> conv_simple j
       | (Some j) -> Fmt.(failwithf "conv_type: type must have string member: %a" pp_json j)
       | None -> []
      )@
      (match assoc_opt "$ref" l with
       | Some (`String s) -> [lookup_ref s]
       | Some j -> Fmt.(failwithf "conv_type: $ref has malformed member: %a" pp_json j)
       | None -> []
      )@
      (match assoc_opt "properties" l with
         Some (`Assoc l) ->
         [Atomic (List.map (fun (k,v) -> Field(k,conv_type0 v)) l)]
       | Some v -> Fmt.(failwithf "conv_type: malformed properties member: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "patternProperties" l with
         Some (`Assoc l) ->
         [Atomic (List.map (fun (k,v) -> FieldRE(k,conv_type0 v)) l)]
       | Some v -> Fmt.(failwithf "conv_type: malformed patternProperties member: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "items" l with
         Some t ->
         [Atomic [ArrayOf (conv_type0 t)]]
       | None -> []
      )@
      (match assoc_opt "uniqueItems" l with
         Some (`Bool true) -> [Atomic [ArrayUnique]]
       | None -> []
      )@
      (match (assoc_opt "minLength" l, assoc_opt "maxLength" l) with
         (None, None) -> []
       | (None, Some _) -> Fmt.(failwithf "conv_type: maxLength requires minLength: %a" pp_json j)
       | (Some min, max) ->
         let min = match min with
             `Int n -> n
           | `Float f -> int_of_float f
           | j -> Fmt.(failwithf "conv_type: minLength must be number: %a" pp_json j) in
         let max = match max with
             None -> None
           | Some (`Int n) -> Some n
           | Some (`Float f) -> Some (int_of_float f)
           | Some j -> Fmt.(failwithf "conv_type: minLength must be number: %a" pp_json j) in
         [And(Simple JString, Atomic [(Size Bound.({it=min; inclusive = true}, {it=max; inclusive = true}))])]
      )@
      (match (assoc_opt "minItems" l, assoc_opt "maxItems" l) with
         (None, None) -> []
       | (min, max) ->
         let max = match max with
             Some (`Int n) -> Some n
           | Some (`Float f) -> Some (int_of_float f)
           | Some j -> Fmt.(failwithf "conv_type: minLength must be number: %a" pp_json j)
           | None -> None in
         let min = match min with
             Some (`Int n) -> n
           | Some (`Float f) -> int_of_float f
           | Some j -> Fmt.(failwithf "conv_type: minLength must be number: %a" pp_json j)
           | None -> 0 in
         [And(Simple JArray, Atomic [(Size Bound.({it=min; inclusive = true}, {it=max; inclusive = true}))])]
      )@
      (match assoc_opt "anyOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
      let (last,l) = sep_last l in
         [List.fold_right (fun a b -> Or(a,b)) l last]
       | Some v -> Fmt.(failwithf "conv_type: anyOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "allOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
      let (last,l) = sep_last l in
         [List.fold_right (fun a b -> And(a,b)) l last]
       | Some v -> Fmt.(failwithf "conv_type: allOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "oneOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
      let (last,l) = sep_last l in
         [List.fold_right (fun a b -> Xor(a,b)) l last]
       | Some v -> Fmt.(failwithf "conv_type: oneOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "additionalProperties" l with
         Some (`Bool b) -> [Atomic[Sealed (not b)]]
       | Some (`Assoc _ as j) ->
         [Atomic [OrElse (andList (conv_type_l j))]]
       | Some v -> Fmt.(failwithf "conv_type: additionalProperties did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "required" l with
         Some (`List [])  ->
         Fmt.(failwithf "conv_type: required had empty array payload: %a" pp_json j)
       | Some (`List l) when List.for_all isString l  ->
         [Atomic[FieldRequired (List.map (function `String s -> s | _ -> assert false) l)]]
       | Some v -> Fmt.(failwithf "conv_type: required did not have nonempty string array payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "enum" l with
         Some (`List l)  ->
         [Atomic[Enum l]]
       | Some v -> Fmt.(failwithf "conv_type: enum did not have array payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "pattern" l with
         Some (`String re)  ->
         [Atomic[StringRE re]]
       | Some v -> Fmt.(failwithf "conv_type: pattern did not have string payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "default" l with
         Some j  -> [Atomic[Default j]]
       | None -> []
      )@
      (match assoc_opt "format" l with
         Some (`String s)  -> [Atomic[Format s]]
       | Some v -> Fmt.(failwithf "conv_type: format did not have string payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "propertyNames" l with
         Some t ->
         [Atomic [PropertyNames (conv_type0 t)]]
       | None -> []
      )@
      (match assoc_opt "not" l with
         Some t ->
         [Not (conv_type0 t)]
       | None -> []
      )

    | j -> Fmt.(failwithf "conv_type: %a" pp_json j)

  and conv_type0 t =
    match conv_type_l t with
      [] -> Fmt.(failwithf "conv_type: conversion produced no result: %a" pp_json t)
    | l -> andList l
  
let conv_type t =
  reset () ;
  let t = conv_type0 t in
  let l = List.map (fun (id, mid) -> Import(id, mid)) !imports in
  let l = if !locals = [] then l else
      l@[Decls(true, List.rev !locals)] in
  if l = [] then Decls(false, [("t", t)])
  else 
    Local(l, [Decls(false, [("t", t)])])

let load_file s =
  if Str.(string_match (regexp ".*\\.json$") s 0) then
    let j = Yojson.Basic.from_file s in
    conv_type j
  else if Str.(string_match (regexp ".*\\.utj$") s 0) then
    match Utparse0.(parse_file parse_utype_structure) s with
      [t] -> t
    | l -> Local([], l)
  else Fmt.(failwithf "load_file: format not recognized: %s" s)
