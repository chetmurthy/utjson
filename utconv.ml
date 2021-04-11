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

  let counter = ref 0
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    MID.of_string mid

  let uri2type = ref []
  let add_uri2type s r = uri2type := (s,r) :: !uri2type

  let imports = ref []
  let add_imports s mid = imports := (s, mid):: !imports 

  let lookup_ref s =
    match List.assoc s !uri2type with
      r -> r
    | exception Not_found ->
      let mid = newmid() in
      let r = Ref(Some (REL mid),"t") in
      add_imports s mid ;
      add_uri2type s r ;
      r

  let locals = ref [] 
  let forward_define_local s =
    add_uri2type (Printf.sprintf "#/definitions/%s" s) (Ref(None, s)) ;
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
    | `String "integer" -> [Ref (Some(REL (MID.of_string "Predefined")), "integer")]
    | `String "scalar" -> [Ref (Some(REL (MID.of_string "Predefined")), "scalar")]
    | v -> Fmt.(failwithf "conv_type: malformed type member: %a" pp_json v)

  let documentation_keys = [
    "$schema"; "$id"; "title"; "description"; "$contact"; "$comment"
    ; "examples"; "example" ; "documentation"; "enumDescriptions"
    ; "deprecated"; "version" ; "authors"
    ; "$vocabulary"
  ]
  let known_keys = documentation_keys@[
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
  ;"if"; "then"; "else"
  ]

  let rec conv_type_l (j : json) = match j with
      `Assoc l when l |> List.for_all (fun (k,_) -> List.mem k documentation_keys) ->
      [Ref (Some(REL (MID.of_string "Predefined")), "json")]
    | `Assoc l ->
      let keys = List.map fst l in
      keys |> List.iter (fun k ->
          if not (List.mem k known_keys) then
            Fmt.(failwithf "conv_type: unrecognized object-key %s in %a" k pp_json j)
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

  and conv_definitions j =
    match j with
      `Assoc l ->
      l |> List.iter (fun (name, _) ->
          forward_define_local name
        ) ;
      l |> List.iter (fun (name, t) ->
          let t = conv_type0 t in
          register_local_definition name t
        )

    | _ -> assert false

let conv_type t =
  reset () ;
  (match t with
     `Assoc l ->
     (match assoc_opt "definitions" l with
        Some (`Assoc _ as j) -> conv_definitions j

      | Some v -> Fmt.(failwithf "conv_type: malformed definitions member: %a" pp_json v)
      | None -> ()
     ) ;
     (match assoc_opt "$defs" l with
        Some (`Assoc _ as j) -> conv_definitions j

      | Some v -> Fmt.(failwithf "conv_type: malformed definitions member: %a" pp_json v)
      | None -> ()
     ) ;
   | _ -> ()) ;
  let t = conv_type0 t in
  let l = List.map (fun (id, mid) -> StImport(id, mid)) !imports in
  let l = if !locals = [] then l else
      l@[StTypes(true, List.rev !locals)] in
  if l = [] then [StTypes(false, [("t", t)])]
  else 
    [StLocal(l, [StTypes(false, [("t", t)])])]

let load_file s =
  if Str.(string_match (regexp ".*\\.json$") s 0) then
    let j = Yojson.Basic.from_file s in
    conv_type j
  else if Str.(string_match (regexp ".*\\.utj$") s 0) then
    Utparse0.(parse_file parse_structure) s
  else Fmt.(failwithf "load_file: format not recognized: %s" s)
