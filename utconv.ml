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

let orList l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> Or(a,b)) l last

let xorList l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> Xor(a,b)) l last

  let assoc_opt k (l : (string * Yojson.Basic.t) list) =
    match List.assoc k l with
      v -> Some v
    | exception Not_found -> None

  let entire_schema = ref `Null
  let counter = ref 0
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    ID.of_string mid

  let definitions_worklist = ref []
  let path2type = ref []

  let find_definition path =
    let rec findrec = function
        ([], j) -> j
      | ("#"::t, j) -> findrec (t, j)
      | (h::t, `Assoc l) -> begin match assoc_opt h l with
            None -> Fmt.(failwithf "cannot find definition %s in schema" (String.concat "/" path))
          | Some j -> findrec(t,j)
        end
      | _ -> Fmt.(failwithf "path %s mismatched with schema" (String.concat "/" path))
    in findrec (path, !entire_schema)

  let typename_of_path path =
    let path = match path with
        "#"::"definitions"::t -> t
      | "#"::"$defs"::t -> t
      | _ -> path in
    let tname = String.concat "_" path in
    ID.of_string tname

  let rec lookup_definition_ref s =
    let path = String.split_on_char '/' s in
    match List.assoc path !path2type with
      v -> v
    | exception Not_found ->
      let def = find_definition path in
      let tname = typename_of_path path in
      let t = Ref(None, tname) in
      Stack.push definitions_worklist (path,def) ;
      Stack.push path2type (path, t) ;
      t

  let uri2type = ref []
  let add_uri2type s r = uri2type := (s,r) :: !uri2type

  let imports = ref []
  let add_imports s mid = imports := (s, mid):: !imports 

  let lookup_import_ref s =
    match List.assoc s !uri2type with
      r -> r
    | exception Not_found ->
      let mid = newmid() in
      let r = Ref(Some (REL mid),(ID.of_string "t")) in
      add_imports s mid ;
      add_uri2type s r ;
      r

  let lookup_ref s =
    if String.get s 0 = '#' then
      lookup_definition_ref s
    else
      lookup_import_ref s

  let reset t = 
    entire_schema := t ;
    definitions_worklist := [] ;
    path2type := [] ;
    counter := 0 ;
    uri2type := [] ;
    imports := []

  let conv_simple = function
      `String "null" -> [Simple JNull]
    | `String "string" -> [Simple JString]
    | `String "boolean" -> [Simple JBool]
    | `String "number" -> [Simple JNumber]
    | `String "array" -> [Simple JArray]
    | `String "object" -> [Simple JObject]
    | `String "integer" -> [Ref (Some(REL (ID.of_string "Predefined")), (ID.of_string "integer"))]
    | `String "scalar" -> [Ref (Some(REL (ID.of_string "Predefined")), (ID.of_string "scalar"))]
    | v -> Fmt.(failwithf "conv_type: malformed type member: %a" pp_json v)

let documentation_keys = [
  "$schema"; "$id"; "title"; "titles"; "description"; "$contact"; "$comment"
  ; "xcomments"
  ; "examples"; "example" ; "documentation"; "enumDescriptions"
  ; "markdownDescription"
  ; "deprecated" ; "deprecationMessage"
  ; "version" ; "authors"
  ; "$vocabulary"
  ; "$xsd-type"
  ; "$xsd-full-type"
  ]
let known_useful_keys = [
    "$ref"
  ; "type"; "properties"; "required"; "patternProperties"
  ; "minimum"; "maximum"
  ; "exclusiveMinimum"; "exclusiveMaximum"
  ; "minLength"; "maxLength"
  ; "items"; "additionalProperties"; "unevaluatedProperties"
  ; "additionalItems"
  ; "minItems"; "maxItems"
  ; "minProperties"; "maxProperties"
  ; "uniqueItems"; "id"
  ; "definitions" ; "$defs"
  ; "enum"; "default";"pattern";"format";"propertyNames"
  ; "anyOf";"allOf";"oneOf";"not"
  ; "contentMediaType";"contentEncoding"
  ; "const" ; "multipleOf"
  ; "dependencies"
  ; "if"; "then"; "else"
  ]
let known_garbage_keys = [
    "x-intellij-case-insensitive"
  ; "x-intellij-language-injection"
  ; "x-intellij-html-description"
  ; "x-intellij-enum-metadata"
  ; "fileMatch"
  ]
let known_keys = documentation_keys@known_useful_keys

  let rec conv_type_l (j : json) = match j with
      `Assoc l when l |> List.for_all (fun (k,_) -> List.mem k documentation_keys) ->
      [Ref (Some(REL (ID.of_string "Predefined")), (ID.of_string "json"))]
    | `Assoc l ->
      let keys = List.map fst l in
      keys |> List.iter (fun k ->
          if not (List.mem k known_keys) then begin
            if List.mem k known_garbage_keys then
              Fmt.(pf stderr "conv_type: known garbage object-key %s in@.%s@.%!" k (Yojson.Basic.pretty_to_string j))
            else
              Fmt.(failwithf "conv_type: unrecognized object-key %s in@.%s@.%!" k (Yojson.Basic.pretty_to_string j))
          end
        ) ;
      (match assoc_opt "type" l with
       | (Some (`String _ as j)) -> conv_simple j
       | (Some (`List (_::_ as l))) when List.for_all isString l ->
         let l = List.concat_map conv_simple l in
         [orList l]
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
         Some (`List l) ->
         [Atomic [ArrayTuple (List.map conv_type0 l)]]
       | Some (`Assoc _ as t) ->
         [Atomic [ArrayOf (conv_type0 t)]]
       | None -> []
      )@
      (match assoc_opt "uniqueItems" l with
         Some (`Bool true) -> [Atomic [ArrayUnique]]
       | Some (`Bool false) -> []
       | None -> []
      )@
      (match (assoc_opt "minLength" l, assoc_opt "maxLength" l) with
         (None, None) -> []
       | (min, max) ->
         let min = match min with
             Some (`Int n) -> n
           | Some (`Float f) -> int_of_float f
           | None -> 0
           | Some j -> Fmt.(failwithf "conv_type: minLength must be number: %a" pp_json j) in
         let max = match max with
             None -> None
           | Some (`Int n) -> Some n
           | Some (`Float f) -> Some (int_of_float f)
           | Some j -> Fmt.(failwithf "conv_type: maxLength must be number: %a" pp_json j) in
         [And(Simple JString, Atomic [(Size Bound.({it=min; exclusive = false}, {it=max; exclusive = false}))])]
      )@
      (match (assoc_opt "minItems" l, assoc_opt "maxItems" l) with
         (None, None) -> []
       | (min, max) ->
         let max = match max with
             Some (`Int n) -> Some n
           | Some (`Float f) -> Some (int_of_float f)
           | Some j -> Fmt.(failwithf "conv_type: maxItems be number: %a" pp_json j)
           | None -> None in
         let min = match min with
             Some (`Int n) -> n
           | Some (`Float f) -> int_of_float f
           | Some j -> Fmt.(failwithf "conv_type: minItems must be number: %a" pp_json j)
           | None -> 0 in
         [And(Simple JArray, Atomic [(Size Bound.({it=min; exclusive = false}, {it=max; exclusive = false}))])]
      )@
      (match (assoc_opt "minProperties" l, assoc_opt "maxProperties" l) with
         (None, None) -> []
       | (min, max) ->
         let max = match max with
             Some (`Int n) -> Some n
           | Some (`Float f) -> Some (int_of_float f)
           | Some j -> Fmt.(failwithf "conv_type: maxProperties must be number: %a" pp_json j)
           | None -> None in
         let min = match min with
             Some (`Int n) -> n
           | Some (`Float f) -> int_of_float f
           | Some j -> Fmt.(failwithf "conv_type: minProperties must be number: %a" pp_json j)
           | None -> 0 in
         [And(Simple JObject, Atomic [(Size Bound.({it=min; exclusive = false}, {it=max; exclusive = false}))])]
      )@
      (match (assoc_opt "minimum" l, assoc_opt "exclusiveMinimum" l,
              assoc_opt "maximum" l, assoc_opt "exclusiveMaximum" l) with
         (None, None, None, None) -> []
       | (min, emin, max, emax) ->
         let min = match (min,emin) with
             (Some _, Some _) ->
             Fmt.(failwithf "conv_type: cannot specify both minimum and exclusiveMinimum: %a" pp_json j)
           | (None, None) ->
             Bound.{it=None; exclusive = false}

           | (Some (`Int n), None) -> Bound.{it=Some (float_of_int n); exclusive = false}
           | (Some (`Float f), None) -> Bound.{it=Some f; exclusive = false}
           | (Some j, None) -> Fmt.(failwithf "conv_type: minimum must be number: %a" pp_json j)

           | (None, Some (`Int n)) -> Bound.{it=Some (float_of_int n); exclusive = true}
           | (None, Some (`Float f)) -> Bound.{it=Some f; exclusive = true}
           | (None, Some j) -> Fmt.(failwithf "conv_type: exclusiveMinimum must be number: %a" pp_json j) in

         let max = match (max,emax) with
             (Some _, Some _) ->
             Fmt.(failwithf "conv_type: cannot specify both maximum and exclusiveMaximum: %a" pp_json j)
           | (None, None) ->
             Bound.{it=None; exclusive = false}

           | (Some (`Int n), None) -> Bound.{it=Some (float_of_int n); exclusive = false}
           | (Some (`Float f), None) -> Bound.{it=Some f; exclusive = false}
           | (Some j, None) -> Fmt.(failwithf "conv_type: maximum must be number: %a" pp_json j)

           | (None, Some (`Int n)) -> Bound.{it=Some (float_of_int n); exclusive = true}
           | (None, Some (`Float f)) -> Bound.{it=Some f; exclusive = true}
           | (None, Some j) -> Fmt.(failwithf "conv_type: exclusiveMaximum must be number: %a" pp_json j) in

         [Atomic [NumberBound(min, max)]]
      )@
      (match assoc_opt "anyOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
         [orList l]
       | Some v -> Fmt.(failwithf "conv_type: anyOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "allOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
         [andList l]
       | Some v -> Fmt.(failwithf "conv_type: allOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "oneOf" l with
         Some (`List (_::_ as l)) ->
         let l = List.map conv_type0 l in
         [xorList l]
       | Some v -> Fmt.(failwithf "conv_type: oneOf did not have nonempty array paylaod: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "additionalProperties" l with
         Some (`Bool b) -> [And(Simple JObject, Atomic[Sealed (not b)])]
       | Some (`Assoc _ as j) ->
         let l = conv_type_l j in
         if l = [] then Fmt.(failwithf "additionalProperties %a yielded no type-constraints" pp_json j) ;
         [Atomic [OrElse (andList l)]]
       | Some v -> Fmt.(failwithf "conv_type: additionalProperties did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "additionalItems" l with
         Some (`Bool b) -> [And(Simple JArray, Atomic[Sealed (not b)])]
       | Some (`Assoc _ as j) ->
         let l = conv_type_l j in
         if l = [] then Fmt.(failwithf "additionalItems %a yielded no type-constraints" pp_json j) ;
         [And(Simple JArray, Atomic [OrElse (andList l)])]
       | Some v -> Fmt.(failwithf "conv_type: additionalItems did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "unevaluatedProperties" l with
         Some (`Bool b) -> [And(Simple JObject, Atomic[Sealed (not b)])]
       | Some (`Assoc _ as j) ->
         let l = conv_type_l j in
         if l = [] then Fmt.(failwithf "unevaluatedProperties %a yielded no type-constraints" pp_json j) ;
         [Atomic [OrElse (andList l)]]
       | Some v -> Fmt.(failwithf "conv_type: unevaluatedProperties did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "required" l with
         Some (`List [])  ->
         []
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
      (match assoc_opt "const" l with
         Some j  ->
         [Atomic[Enum [j]]]
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
      )@
      (match assoc_opt "contentMediaType" l with
         Some (`String s)  -> [Atomic[ContentMediaType s]]
       | Some v -> Fmt.(failwithf "conv_type: contentMediaType did not have string payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "contentEncoding" l with
         Some (`String s)  -> [Atomic[ContentEncoding s]]
       | Some v -> Fmt.(failwithf "conv_type: contentEncoding did not have string payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "dependencies" l with
         Some (`Assoc [])  -> []
       | Some (`Assoc l)  ->
         let l = List.map (fun (k, v) ->
             match v with
               `List fl when List.for_all isString fl ->
               let fl = List.map (function `String s -> s | _ -> assert false) fl in
               Impl(Atomic[FieldRequired [k]], Atomic[FieldRequired fl])
             | (`Assoc _) as j ->
               Impl(Atomic[FieldRequired [k]], conv_type0 j)
             | v ->  Fmt.(failwithf "conv_type: rhs of a dependency was neither array nor object: %a" pp_json v)
           ) l in
         [andList l]
       | Some v -> Fmt.(failwithf "conv_type: dependencies did not have object payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "multipleOf" l with
         Some (`Int n)  -> [Atomic[MultipleOf (float_of_int n)]]
       | Some (`Float n)  -> [Atomic[MultipleOf n]]
       | Some v -> Fmt.(failwithf "conv_type: multipleOf did not have number payload: %a" pp_json v)
       | None -> []
      )@
      (match (assoc_opt "if" l,assoc_opt "then" l,assoc_opt "else" l) with
         (Some ifj, Some thenj, Some elsej) ->
         let ift = conv_type0 ifj in
         [And(Impl(ift, conv_type0 thenj),
              Impl(ift, conv_type0 elsej))]

       | (Some ifj, Some thenj, None) ->
         let ift = conv_type0 ifj in
         [Impl(ift, conv_type0 thenj)]

       | (Some _, None, Some _) ->
         Fmt.(failwithf "conv_type: if-else with no then: %a" pp_json j)

       | (None, Some _, None) ->
         Fmt.(failwithf "conv_type: then with no if: %a" pp_json j)

       | (None, None, Some _) ->
         Fmt.(failwithf "conv_type: else with no if: %a" pp_json j)

       | (None,None,None) -> []
      )

    | j -> Fmt.(failwithf "conv_type: %a" pp_json j)

  and conv_type0 t =
    match conv_type_l t with
      [] -> Fmt.(failwithf "conv_type: conversion produced no result: %a" pp_json t)
    | l -> andList l

  let conv_type j = conv_type0 j

  let conv_definitions () =
    let rec drec acc =
      if Stack.empty definitions_worklist then
        List.stable_sort Stdlib.compare acc
      else
        let (path, def) = Stack.top definitions_worklist in
        Stack.pop definitions_worklist ;
        let def = conv_type def in
        let tname = typename_of_path path in
        drec ((tname, def)::acc)
    in
    drec []

let conv_schema t =
  reset t ;
  let t = conv_type t in
  let l = List.map (fun (id, mid) -> StImport(id, mid)) !imports in
  let defs = conv_definitions() in
  let l = if defs <> [] then
      (StTypes(true, defs))::l
    else l in
  if l = [] then [StTypes(false, [(ID.of_string "t", t)])]
  else 
    [StLocal(l, [StTypes(false, [(ID.of_string "t", t)])])]

let load_file s =
  if Str.(string_match (regexp ".*\\.json$") s 0) then
    let j = Yojson.Basic.from_file s in
    conv_schema j
  else if Str.(string_match (regexp ".*\\.utj$") s 0) then
    Utparse0.(parse_file parse_structure) s
  else Fmt.(failwithf "load_file: format not recognized: %s" s)
