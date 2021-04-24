open Pa_ppx_utils.Std

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

let is_capitalized s =
  let c0 = String.get s 0 in
  match c0 with
  'A'..'Z' -> true | _ -> false

  let clean_path s =
    let s = Str.(substitute_first (regexp (quote "#/$defs/")) (fun _ -> "") s) in
    let s = Str.(substitute_first (regexp (quote "#/defs/")) (fun _ -> "") s) in
    let s = Str.(substitute_first (regexp (quote "#/refs/")) (fun _ -> "") s) in
    let s = Str.(substitute_first (regexp (quote "#/definitions/")) (fun _ -> "") s) in
    let s = Str.(global_substitute (regexp "[@#:/.$-]") (fun _ -> "_") s) in
    if is_capitalized s then
      "_"^s
    else s

  let typename_of_path s =
    let s = clean_path s in
    ID.of_string s

(** Conversion Context

    This an attempt to somewhat automate the process of converting
   files from JSON Schema to UTJ, and make it easy to distinguish
   between converted files that we need to preserve (b/c hand-edited)
   and those that were converted and cached.

    Method:

    (0) if uri starts with "http://", replace that with "http-schema-cache"

    (1) first, replace ".json" with ".utj" before proceeding.

    (2) search for file on filepath

    (3) and if that fails, then map original uri thru urimap.

 *)

module CC = struct
  type t = {
    filename : string
  ; httpcache : string
  ; filepath : string list
  ; urimap : (string * string) list
  ; verbose : bool
  }
[@@deriving show { with_path = false },eq]


  let mk ?(verbose=true) ?(httpcache="http-schema-cache") ?(filename="") ?(filepath=[]) ?(urimap=[]) () =
    { filename ; filepath ; httpcache ; urimap ; verbose }

  let with_filename cc filename = { cc with filename }

  let map_uri t s =
    let http_colon_slash_slash = "http://" in
    let https_colon_slash_slash = "https://" in
    let s' =
      if starts_with ~pat:http_colon_slash_slash s then
        t.httpcache^"/"^(string_suffix s (String.length http_colon_slash_slash))
      else if starts_with ~pat:https_colon_slash_slash s then
        t.httpcache^"/"^(string_suffix s (String.length https_colon_slash_slash))
      else s in
    let uri = Fpath.v s' in
    (* (1) first get extension right *)
    let utj = if Fpath.has_ext "utj" uri then uri
      else if Fpath.has_ext "json" uri then
        Fpath.(uri |> rem_ext |> add_ext "utj")
      else
        Fpath.(uri |> add_ext "utj") in

    match t.filepath |> List.find_map (fun dir ->
          let utj' = Fpath.(append (v dir) utj) in
          if utj' |> Bos.OS.File.exists |> Rresult.R.get_ok then
            Some (utj' |> Fpath.to_string)
          else None) with
      Some s -> ("filepath", s)
    | None ->
      match List.assoc s t.urimap with
        v -> ("urimap", v)
      | exception Not_found ->
        Fmt.(failwithf "CC.map_uri: uri %a not found@.CC: %a@."
               Dump.string s
               pp t)

  let map_url t url =
    let (uri, typename) = match String.split_on_char '#' url with
        ([s] | [s;""]) -> (s, None)
      | [uri;fragment] -> (uri, Some (typename_of_path ("#"^fragment))) in
    let (via, utj_uri) = map_uri t uri in
    if t.verbose then
      Fmt.(pf stderr "[%s -> %s via %s]\n%!" uri utj_uri via) ;
    (utj_uri, typename)
end
module ConvContext = CC

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

  let assoc_opt (k: string) l =
    match List.assoc k l with
      v -> Some v
    | exception Not_found -> None

  let cc = ref (CC.mk ())
  let entire_schema = ref `Null
  let counter = ref 0
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    ID.of_string mid

  let id2node = ref []
  let definitions_worklist = ref []
  let path2type = ref []

  let extract_id2node (schema : Yojson.Basic.t) =
    let acc = ref [] in
    let rec prec j = match j with
        `Assoc l ->
        if List.mem_assoc "$id" l then
          begin match List.assoc "$id" l with
              `String id ->
              Stack.push acc (id, j)
            | _ -> ()
          end ;
        List.iter (fun (_, v) -> prec v) l

      | `List l -> List.iter prec l
      | _ -> ()
    in prec schema;
    !acc

  let populate_id2node schema =
    id2node := extract_id2node schema

  let find_definition path =
    let rec findrec = function
        ([], j) -> Some j
      | ("#"::t, j) -> findrec (t, j)
      | (h::t, `Assoc l) -> begin match assoc_opt h l with
            None -> None
          | Some j -> findrec(t,j)
        end
      | (h::t, (`List l as j)) ->
        let n = try int_of_string h with Failure _ ->
          Fmt.(failwithf "path-step %a not an integer, but object was an array:@.%s@."
                 Dump.string h (Yojson.Basic.pretty_to_string j)) in
        if n < 0 || n >= List.length l then
          Fmt.(failwithf "path-step %d was out-of-bounds for object@.%s@."
                 n (Yojson.Basic.pretty_to_string j)) ;
        findrec (t, List.nth l n)
      | _ -> Fmt.(failwithf "path %s mismatched with schema" path)
    in findrec (String.split_on_char '/' path, !entire_schema)

  let assoc_path2type s = assoc_opt s !path2type
  let add_path2type (s,t) =
    Stack.push path2type (s, t)

  let rec lookup_definition_ref s =
    match assoc_path2type s with
      Some v -> v
    | None ->
      let def = match (find_definition s, assoc_opt s !id2node) with
          (None, None) -> Fmt.(failwithf "lookup_definition_ref: cannot find reference %a" Dump.string s)
        | (Some def, Some def') ->
          if def <> def' then
            Fmt.(failwithf "lookup_definition_ref: error: reference %a is both an ID and a path, and they point at different things!" Dump.string s) ;
          def
        | (Some def, None) -> def
        | (None, Some def) -> def in
      let tname = typename_of_path s in
      let t = Ref(None, tname) in
      Stack.push definitions_worklist (s,def) ;
      add_path2type (s,t) ;
      t

  let imports = ref []
  let add_imports s mid = imports := (s, mid):: !imports 

  let lookup_import_ref s =
    let (filename, typename) = CC.map_url !cc s in
    let mid = match List.assoc filename !imports with
        mid -> mid
      | exception Not_found ->
        let mid = newmid() in
        add_imports filename mid ;
        mid in begin match typename with
        None -> 
        Ref(Some (REL mid), ID.of_string "t")
      | Some tname ->
        Ref(Some (DEREF (REL mid, ID.of_string "DEFINITIONS")), tname)
    end

  let lookup_ref s =
    if s = "#" then
      Fmt.(failwithf "Cannot have a ref %a: need at least some kind of name; use $id" Dump.string s) ;
    if String.get s 0 = '#' then
      lookup_definition_ref s
    else
      lookup_import_ref s

  let reset newcc t = 
    cc := newcc ;
    entire_schema := t ;
    definitions_worklist := [] ;
    path2type := [] ;
    counter := 0 ;
    imports := [] ;
    populate_id2node t

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
  ; "meta" ; "metatag" ; "PWA"
  ; "tsType"
  ; "defaultSnippets"
  ; "@comment"
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
  ; "definitions" ; "$defs"; "defs" ; "refs"
  ; "enum"; "default";"pattern";"format";"propertyNames"
  ; "anyOf";"allOf";"oneOf";"not"
  ; "contentMediaType";"contentEncoding"
  ; "const" ; "multipleOf"
  ; "if"; "then"; "else"
  ; "dependencies"
  ]
let known_garbage_keys = [
    "x-intellij-case-insensitive"
  ; "x-intellij-language-injection"
  ; "x-intellij-html-description"
  ; "x-intellij-enum-metadata"
  ; "x-kubernetes-embedded-resource"
  ; "x-kubernetes-group-version-kind"
  ; "x-kubernetes-int-or-string"
  ; "x-kubernetes-list-map-keys"
  ; "x-kubernetes-list-type"
  ; "x-kubernetes-map-type"
  ; "x-kubernetes-patch-merge-key"
  ; "x-kubernetes-patch-strategy"
  ; "x-kubernetes-preserve-unknown-fields"
  ; "x-kubernetes-unions"
  ; "fileMatch"
  ]
let known_keys = documentation_keys@known_useful_keys

  let rec conv_type_l (j : json) = match j with
      `Assoc l when l |> List.for_all (fun (k,_) -> List.mem k documentation_keys) ->
      ([Ref (Some(REL (ID.of_string "Predefined")), (ID.of_string "json"))], [])
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
      let l1 = 
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
         Some (`Assoc []) -> []
       | Some (`Assoc l) ->
         [Atomic (List.rev (List.rev_map (fun (k,v) -> Field(k,conv_type0 v)) l))]
       | Some v -> Fmt.(failwithf "conv_type: malformed properties member: %a" pp_json v)
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
         [Atomic [(Size Bound.({it=min; exclusive = false}, {it=max; exclusive = false}))]]
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
         [Atomic [(Size Bound.({it=min; exclusive = false}, {it=max; exclusive = false}))]]
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
         [Atomic[Enum (List.map canon_json l)]]
       | Some v -> Fmt.(failwithf "conv_type: enum did not have array payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "const" l with
         Some j  ->
         [Atomic[Enum [canon_json j]]]
       | None -> []
      )@
      (match assoc_opt "pattern" l with
         Some (`String re)  ->
         if re <> "" then
           [Atomic[StringRE re]]
         else [Simple JString]
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
              Impl(Not ift, conv_type0 elsej))]

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
      in
      let l2 =
        (match assoc_opt "patternProperties" l with
         Some (`Assoc l) ->
         (List.map (fun (k,v) ->
              let ut = conv_type0 v in
              if k <> "" then
                FieldRE(k,ut)
              else OrElse ut
            ) l)
       | Some v -> Fmt.(failwithf "conv_type: malformed patternProperties member: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "additionalProperties" l with
         Some (`Bool false) -> [Sealed]
       | Some (`Bool true) -> []
       | Some (`Assoc _ as j) -> begin
           match conv_type_l j with
             ([], _) -> Fmt.(failwithf "additionalProperties %a yielded no type-constraints" pp_json j)
           | _ ->
             [OrElse (conv_type0 j)]
       end
       | Some v -> Fmt.(failwithf "conv_type: additionalProperties did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "additionalItems" l with
         Some (`Bool false) -> [Sealed]
       | Some (`Bool true) -> []
       | Some (`Assoc _ as j) -> begin
           match conv_type_l j with
             ([], _) -> Fmt.(failwithf "additionalItems %a yielded no type-constraints" pp_json j)
           | _ ->
             [OrElse (conv_type0 j)]
       end
       | Some v -> Fmt.(failwithf "conv_type: additionalItems did not have bool or type payload: %a" pp_json v)
       | None -> []
      )@
      (match assoc_opt "unevaluatedProperties" l with
         Some (`Bool false) -> [Sealed]
       | Some (`Assoc _ as j) -> begin
           match conv_type_l j with
             ([], _) -> Fmt.(failwithf "unevaluatedProperties %a yielded no type-constraints" pp_json j)
           | _ ->
             [OrElse (conv_type0 j)]
       end
       | Some v -> Fmt.(failwithf "conv_type: unevaluatedProperties did not have bool or type payload: %a" pp_json v)
       | None -> []
      )
      in
      (l1,l2)

    | j -> Fmt.(failwithf "conv_type: expected an object but got@.%s@." (Yojson.Basic.pretty_to_string j))

  and wrap_seal j ut l2 =
    let patterns = l2 |> List.filter_map (function
          FieldRE(re,ut) -> Some(re, ut)
        | _ -> None
      ) in
    let orelse = match l2 |> List.filter_map (function
          OrElse ut -> Some ut
        | _ -> None
      ) with
      [] -> None
    | [ut] -> Some ut
    | _ -> Fmt.(failwithf "conv_type: type had more than one Orelse: %s"
                  (Yojson.Basic.pretty_to_string j)) in
    let isSealed = List.mem Sealed l2 in
    let addSealed =
      patterns <> [] || orelse <> None || isSealed in
      if addSealed then
        Seal(ut, patterns, orelse)
      else ut

  and conv_type0 ?(allow_empty=false) t =
    match conv_type_l t with
      ([],[]) ->
      if allow_empty then begin
        Fmt.(pf stderr "WARNING: conv_type: conversion produced no result: %s" (Yojson.Basic.pretty_to_string t)) ;
        UtTrue
      end
      else
        Fmt.(failwithf "conv_type: conversion produced no result: %s" (Yojson.Basic.pretty_to_string t))
    | ([],l2) ->
      Fmt.(pf stderr "WARNING: conv_type: conversion produced no type, but FieldRE/Orelse : %s"
             (Yojson.Basic.pretty_to_string t)) ;
      wrap_seal t UtFalse l2

    | (l,[]) -> andList l
    | (l1,l2) ->
      let ut = andList l1 in
      wrap_seal t ut l2

  let conv_type j = conv_type0 j
  let top_conv_type t = conv_type0 ~allow_empty:true t

  let conv_definitions () =
    let rec drec acc =
      if Stack.empty definitions_worklist then
        List.stable_sort Stdlib.compare acc
      else
        let (path, def) = Stack.top definitions_worklist in
        Stack.pop definitions_worklist ;
        let def = conv_type def in
        let tname = typename_of_path path in
        let isSealed = match def with Seal _ -> true | _ -> false in
        drec ((tname, isSealed, def)::acc)
    in
    drec []

let recognize_definition t = match t with
  `Assoc l ->
  l |> List.exists (function (k, v) ->
      List.mem k known_useful_keys
    )
  | _ -> false

let recognize_definitions cc (t : Yojson.Basic.t) = match t with
    `Assoc l -> begin match (assoc_opt "definitions" l, assoc_opt "$defs" l, assoc_opt "defs" l ) with
        (Some (`Assoc l), None, None) ->
        l |> List.iter (function (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref (Printf.sprintf "#/definitions/%s" k))
          )
      | (None, Some (`Assoc l), None) ->
        l |> List.iter (fun (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref (Printf.sprintf "#/$defs/%s" k))
          )
      | (None, None, Some (`Assoc l)) ->
        l |> List.iter (fun (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref (Printf.sprintf "#/defs/%s" k))
          )
      | (None, None, None) -> ()
      | _ -> Fmt.(failwithf "recognize_definitions: malformed %s" (Yojson.Basic.pretty_to_string t))
    end
  | _ -> ()

let conv_schema cc t =
  reset cc t ;
  recognize_definitions cc t ;
  let t = top_conv_type t in
  let defs = conv_definitions() in
  let defs = List.stable_sort Stdlib.compare defs in
  let l = List.map (fun (id, mid) -> StImport(id, mid)) !imports in
  let l = if defs <> [] then
      l@[
        StModuleBinding(ID.of_string "DEFINITIONS", MeStruct [StTypes(true, defs)])
      ; StOpen(REL (ID.of_string "DEFINITIONS"), None)
      ]
    else l in
  let isSealed = match t with Seal _ -> true | _ -> false in
  l@[StTypes(false, [(ID.of_string "t", isSealed, t)])]

let convert_file ?(with_predefined=false) cc s =
  let open Fpath in
  let stl =
    if has_ext "json" (v s) then
      let j = Yojson.Basic.from_file s in
      conv_schema (CC.with_filename cc s) j
    else Fmt.(failwithf "convert_file: format not recognized: %s" s) in
  if with_predefined then
  [StImport("lib/predefined.utj", ID.of_string "Predefined")]@stl
  else stl
