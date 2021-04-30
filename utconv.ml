open Pa_ppx_utils.Std

open Ututil
open Utypes
open Utypes.LJ
open Utparse0

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

let isString j = match j with
  String _ -> true | _ -> false

let andList loc l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> And(loc,a,b)) l last

let orList loc l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> Or(loc,a,b)) l last

let xorList loc l =
  let (last,l) = sep_last l in
  List.fold_right (fun a b -> Xor(loc, a,b)) l last

  let assoc_opt (k: string) l =
    match List.assoc k l with
      v -> Some v
    | exception Not_found -> None

  let cc = ref (CC.mk ())
  let entire_schema = ref (Null Ploc.dummy)
  let counter = ref 0
  let newmid () =
    let mid = Printf.sprintf "M%d" !counter in
    counter := 1 + !counter ;
    ID.of_string mid

  let id2node = ref []
  let definitions_worklist = ref []
  let path2type = ref []

  let extract_id2node schema =
    let acc = ref [] in
    let rec prec j = match j with
        Assoc (_, l) ->
        if List.mem_assoc "$id" l then
          begin match List.assoc "$id" l with
              String (_, id) ->
              Stack.push acc (id, j)
            | _ -> ()
          end ;
        List.iter (fun (_, v) -> prec v) l

      | List (_, l) -> List.iter prec l
      | _ -> ()
    in prec schema;
    !acc

  let populate_id2node schema =
    id2node := extract_id2node schema

  let find_definition path =
    let rec findrec = function
        ([], j) -> Some j
      | ("#"::t, j) -> findrec (t, j)
      | (h::t, Assoc (_, l)) -> begin match assoc_opt h l with
            None -> None
          | Some j -> findrec(t,j)
        end
      | (h::t, (List (loc, l) as j)) ->
        let n = try int_of_string h with Failure _ ->
          Fmt.(raise_failwithf loc "path-step %a not an integer, but object was an array" Dump.string h) in
        if n < 0 || n >= List.length l then
          Fmt.(raise_failwithf loc "path-step %d was out-of-bounds for object" n) ;
        findrec (t, List.nth l n)
      | _ -> Fmt.(failwithf "path %s mismatched with schema" path)
    in findrec (String.split_on_char '/' path, !entire_schema)

  let assoc_path2type s = assoc_opt s !path2type
  let add_path2type (s,t) =
    Stack.push path2type (s, t)

  let rec lookup_definition_ref loc s =
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
      let t = Ref(loc, (None, tname)) in
      Stack.push definitions_worklist (s,def) ;
      add_path2type (s,t) ;
      t

  let imports = ref []
  let add_imports s mid = imports := (s, mid):: !imports 

  let lookup_import_ref loc s =
    let (filename, typename) = CC.map_url !cc s in
    let mid = match List.assoc filename !imports with
        mid -> mid
      | exception Not_found ->
        let mid = newmid() in
        add_imports filename mid ;
        mid in begin match typename with
        None -> 
        Ref(loc, (Some (REL mid), ID.of_string "t"))
      | Some tname ->
        Ref(loc, (Some (DEREF (REL mid, ID.of_string "DEFINITIONS")), tname))
    end

  let lookup_ref loc s =
    if s = "#" then
      Fmt.(failwithf "Cannot have a ref %a: need at least some kind of name; use $id" Dump.string s) ;
    if String.get s 0 = '#' then
      lookup_definition_ref loc s
    else
      lookup_import_ref loc s

  let reset newcc t = 
    cc := newcc ;
    entire_schema := t ;
    definitions_worklist := [] ;
    path2type := [] ;
    counter := 0 ;
    imports := [] ;
    populate_id2node t

  let conv_simple j = match j with
      String (loc, "null") -> [Simple (loc, JNull)]
    | String (loc, "string") -> [Simple (loc, JString)]
    | String (loc, "boolean") -> [Simple (loc, JBool)]
    | String (loc, "number") -> [Simple (loc, JNumber)]
    | String (loc, "array") -> [Simple (loc, JArray)]
    | String (loc, "object") -> [Simple (loc, JObject)]
    | String (loc, "integer") -> [Ref (loc, (Some(REL (ID.of_string "Predefined")), (ID.of_string "integer")))]
    | String (loc, "scalar") -> [Ref (loc, (Some(REL (ID.of_string "Predefined")), (ID.of_string "scalar")))]
    | v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: malformed type member")

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

  let rec conv_type_l j = match j with
      Assoc (loc, l) when l |> List.for_all (fun (k,_) -> List.mem k documentation_keys) ->
      ([Ref (loc, (Some(REL (ID.of_string "Predefined")), (ID.of_string "json")))], ([], []))
    | Assoc (loc, l) ->
      let keys = List.map fst l in
      keys |> List.iter (fun k ->
          if not (List.mem k known_keys) then begin
            if List.mem k known_garbage_keys then
              Fmt.(pf stderr "%s: conv_type: known garbage object-key %s"
                     (Ploc.string_of_location loc)
                     k)
            else
              Fmt.(raise_failwithf loc "conv_type: unrecognized object-key %s" k)
          end
        ) ;
      let l1 = 
      (match assoc_opt "type" l with
       | (Some (String _ as j)) -> conv_simple j
       | (Some (List (loc, (_::_ as l)))) when List.for_all isString l ->
         let l = List.concat_map conv_simple l in
         [orList loc l]
       | (Some j) -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: type must have string member")
       | None -> []
      )@
      (match assoc_opt "$ref" l with
       | Some (String (loc, s)) -> [lookup_ref loc s]
       | Some j -> Fmt.(raise_failwithf loc "conv_type: $ref has malformed member")
       | None -> []
      )@
      (match assoc_opt "properties" l with
         Some (Assoc (_, [])) -> []
       | Some (Assoc (loc, l)) ->
         [Atomic (loc, List.rev (List.rev_map (fun (k,v) -> Field(LJ.to_loc v, k,conv_type0 v)) l))]
       | Some v -> Fmt.(raise_failwithf loc "conv_type: malformed properties member")
       | None -> []
      )@
      (match assoc_opt "items" l with
         Some (List (loc, l)) ->
         [Atomic (loc, [ArrayTuple (loc, List.map conv_type0 l)])]
       | Some (Assoc (loc, _) as t) ->
         [Atomic (loc, [ArrayOf (loc, conv_type0 t)])]
       | None -> []
      )@
      (match assoc_opt "uniqueItems" l with
         Some (Bool (loc, true)) -> [Atomic (loc, [ArrayUnique loc])]
       | Some (Bool (_, false)) -> []
       | None -> []
      )@
      (match (assoc_opt "minLength" l, assoc_opt "maxLength" l) with
         (None, None) -> []
       | (min, max) ->
         let (min,min_loc_opt) = match min with
             Some (Int (loc, n)) -> (n, Some loc)
           | Some (Float (loc, f)) -> (int_of_float f, Some loc)
           | None -> (0, None)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: minLength must be number") in
         let (max, max_loc_opt) = match max with
             None -> (None, None)
           | Some (Int (loc, n)) -> (Some n, Some loc)
           | Some (Float (loc, f)) -> (Some (int_of_float f), Some loc)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: maxLength must be number") in
         let loc = match (min_loc_opt, max_loc_opt) with
             (None, None) -> Ploc.dummy
           | (Some loc, None) -> loc
           | (None, Some loc) -> loc
           | (Some loc1, Some loc2) -> Ploc.encl loc1 loc2 in
         [Atomic (loc, [(Size (loc, Bound.({it=min; exclusive = false}, {it=max; exclusive = false})))])]
      )@
      (match (assoc_opt "minItems" l, assoc_opt "maxItems" l) with
         (None, None) -> []
       | (min, max) ->
         let (max, max_loc_opt) = match max with
             Some (Int (loc, n)) -> (Some n, Some loc)
           | Some (Float (loc, f)) -> (Some (int_of_float f), Some loc)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: maxItems be number")
           | None -> (None, None) in
         let (min, min_loc_opt) = match min with
             Some (Int (loc, n)) -> (n, Some loc)
           | Some (Float (loc, f)) -> (int_of_float f, Some loc)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: minItems must be number")
           | None -> (0, None) in
         let loc = match (min_loc_opt, max_loc_opt) with
             (None, None) -> Ploc.dummy
           | (Some loc, None) -> loc
           | (None, Some loc) -> loc
           | (Some loc1, Some loc2) -> Ploc.encl loc1 loc2 in
         [Atomic (loc, [(Size (loc, Bound.({it=min; exclusive = false}, {it=max; exclusive = false})))])]
      )@
      (match (assoc_opt "minProperties" l, assoc_opt "maxProperties" l) with
         (None, None) -> []
       | (min, max) ->
         let (max, max_loc_opt) = match max with
             Some (Int (loc, n)) -> (Some n, Some loc)
           | Some (Float (loc, f)) -> (Some (int_of_float f), Some loc)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: maxProperties must be number")
           | None -> (None, None) in
         let (min, min_loc_opt) = match min with
             Some (Int (loc, n)) -> (n, Some loc)
           | Some (Float (loc, f)) -> (int_of_float f, Some loc)
           | Some j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: minProperties must be number")
           | None -> (0, None) in
         let loc = match (min_loc_opt, max_loc_opt) with
             (None, None) -> Ploc.dummy
           | (Some loc, None) -> loc
           | (None, Some loc) -> loc
           | (Some loc1, Some loc2) -> Ploc.encl loc1 loc2 in
         [And(loc, Simple (loc, JObject), Atomic (loc, [(Size (loc, Bound.({it=min; exclusive = false}, {it=max; exclusive = false})))]))]
      )@
      (match (assoc_opt "minimum" l, assoc_opt "exclusiveMinimum" l,
              assoc_opt "maximum" l, assoc_opt "exclusiveMaximum" l) with
         (None, None, None, None) -> []
       | (min, emin, max, emax) ->
         let (min, min_loc_opt) = match (min,emin) with
             (Some j1, Some j2) ->
             let loc = Ploc.encl (LJ.to_loc j1) (LJ.to_loc j2) in
             Fmt.(raise_failwithf loc "conv_type: cannot specify both minimum and exclusiveMinimum")
           | (None, None) ->
             (Bound.{it=None; exclusive = false}, None)

           | (Some (Int (loc, n)), None) -> (Bound.{it=Some (float_of_int n); exclusive = false}, Some loc)
           | (Some (Float (loc, f)), None) -> (Bound.{it=Some f; exclusive = false}, Some loc)
           | (Some j, None) -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: minimum must be number")

           | (None, Some (Int (loc, n))) -> (Bound.{it=Some (float_of_int n); exclusive = true}, Some loc)
           | (None, Some (Float (loc, f))) -> (Bound.{it=Some f; exclusive = true}, Some loc)
           | (None, Some j) -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: exclusiveMinimum must be number") in

         let (max, max_loc_opt) = match (max,emax) with
             (Some j1, Some j2) ->
             let loc = Ploc.encl (LJ.to_loc j1) (LJ.to_loc j2) in
             Fmt.(raise_failwithf loc "conv_type: cannot specify both maximum and exclusiveMaximum")
           | (None, None) ->
             (Bound.{it=None; exclusive = false}, None)

           | (Some (Int (loc, n)), None) -> (Bound.{it=Some (float_of_int n); exclusive = false}, Some loc)
           | (Some (Float (loc, f)), None) -> (Bound.{it=Some f; exclusive = false}, Some loc)
           | (Some j, None) -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: maximum must be number")

           | (None, Some (Int (loc, n))) -> (Bound.{it=Some (float_of_int n); exclusive = true}, Some loc)
           | (None, Some (Float (loc, f))) -> (Bound.{it=Some f; exclusive = true}, Some loc)
           | (None, Some j) -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: exclusiveMaximum must be number") in
         let loc = match (min_loc_opt, max_loc_opt) with
             (None, None) -> Ploc.dummy
           | (Some loc, None) -> loc
           | (None, Some loc) -> loc
           | (Some loc1, Some loc2) -> Ploc.encl loc1 loc2 in

         [Atomic (loc, [NumberBound(loc, (min, max))])]
      )@
      (match assoc_opt "anyOf" l with
         Some (List (loc, (_::_ as l))) ->
         let l = List.map conv_type0 l in
         [orList loc l]
       | Some v -> Fmt.(raise_failwithf loc "conv_type: anyOf did not have nonempty array payload")
       | None -> []
      )@
      (match assoc_opt "allOf" l with
         Some (List (loc, (_::_ as l))) ->
         let l = List.map conv_type0 l in
         [andList loc l]
       | Some v -> Fmt.(raise_failwithf loc "conv_type: allOf did not have nonempty array payload")
       | None -> []
      )@
      (match assoc_opt "oneOf" l with
         Some (List (loc, (_::_ as l))) ->
         let l = List.map conv_type0 l in
         [xorList loc l]
       | Some v -> Fmt.(raise_failwithf loc "conv_type: oneOf did not have nonempty array payload")
       | None -> []
      )@
      (match assoc_opt "required" l with
         Some (List (_, []))  -> []
       | Some (List (loc, l)) when List.for_all isString l  ->
         [Atomic(loc, [FieldRequired (loc, List.map (function String (_, s) -> s | _ -> assert false) l)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: required did not have nonempty string array payload")
       | None -> []
      )@
      (match assoc_opt "enum" l with
         Some (List (loc, l))  ->
         [Atomic(loc, [Enum (loc, l |> List.map LJ.to_json |> List.map canon_json)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: enum did not have array payload")
       | None -> []
      )@
      (match assoc_opt "const" l with
         Some j  ->
         let loc = LJ.to_loc j in
         [Atomic(loc, [Enum (loc, [canon_json (LJ.to_json j)])])]
       | None -> []
      )@
      (match assoc_opt "pattern" l with
         Some (String (loc, re))  ->
         if re <> "" then
           [Atomic(loc, [StringRE (loc, re)])]
         else [Simple (loc, JString)]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: pattern did not have string payload")
       | None -> []
      )@
      (match assoc_opt "default" l with
         Some j  ->
         let loc = LJ.to_loc j in
         [Atomic(loc, [Default (loc, j |> LJ.to_json)])]
       | None -> []
      )@
      (match assoc_opt "format" l with
         Some (String (loc, s))  -> [Atomic(loc, [Format (loc, s)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: format did not have string payload")
       | None -> []
      )@
      (match assoc_opt "propertyNames" l with
         Some t ->
         let loc = LJ.to_loc t in
         [Atomic (loc, [PropertyNames (loc, conv_type0 t)])]
       | None -> []
      )@
      (match assoc_opt "not" l with
         Some t ->
         let loc = LJ.to_loc t in
         [Not (loc, conv_type0 t)]
       | None -> []
      )@
      (match assoc_opt "contentMediaType" l with
         Some (String (loc, s))  -> [Atomic(loc, [ContentMediaType (loc, s)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: contentMediaType did not have string payload")
       | None -> []
      )@
      (match assoc_opt "contentEncoding" l with
         Some (String (loc, s))  -> [Atomic(loc, [ContentEncoding (loc, s)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: contentEncoding did not have string payload")
       | None -> []
      )@
      (match assoc_opt "dependencies" l with
         Some (Assoc (_, []))  -> []
       | Some (Assoc (loc, l))  ->
         let l = List.map (fun (k, v) ->
             match v with
               List (loc, fl) when List.for_all isString fl ->
               let fl = List.map (function String (_, s) -> s | _ -> assert false) fl in
               Impl(loc, Atomic(loc, [FieldRequired (loc, [k])]), Atomic(loc, [FieldRequired (loc, fl)]))
             | (Assoc (loc, _)) as j ->
               Impl(loc, Atomic(loc, [FieldRequired (loc, [k])]), conv_type0 j)
             | v ->  Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: rhs of a dependency was neither array nor object")
           ) l in
         [andList loc l]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: dependencies did not have object payload")
       | None -> []
      )@
      (match assoc_opt "multipleOf" l with
         Some (Int (loc, n))  -> [Atomic(loc, [MultipleOf (loc, float_of_int n)])]
       | Some (Float (loc, n))  -> [Atomic(loc, [MultipleOf (loc, n)])]
       | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: multipleOf did not have number payload")
       | None -> []
      )@
      (match (assoc_opt "if" l,assoc_opt "then" l,assoc_opt "else" l) with
         (Some ifj, Some thenj, Some elsej) ->
         let ift = conv_type0 ifj in
         let loc = LJ.to_loc ifj in
         [And(loc, Impl(loc, ift, conv_type0 thenj),
              Impl(loc, Not (loc, ift), conv_type0 elsej))]

       | (Some ifj, Some thenj, None) ->
         let ift = conv_type0 ifj in
         let loc = LJ.to_loc ifj in
         [Impl(loc, ift, conv_type0 thenj)]

       | (Some j, None, Some _) ->
         Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: if-else with no then")

       | (None, Some j, None) ->
         Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: then with no if")

       | (None, None, Some j) ->
         Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: else with no if")

       | (None,None,None) -> []
      )
      in
      let l2_additional = ref []
      and l2_orelse = ref [] in begin
        (match assoc_opt "patternProperties" l with
           Some (Assoc (loc, l)) ->
           l |> List.iter (fun (k,v) ->
               let ut = conv_type0 v in
               if k <> "" then
                 Stack.push l2_additional (k,ut)
               else Stack.push l2_orelse ut
             )
         | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: malformed patternProperties member")
         | None -> ()
        );
        (match assoc_opt "additionalProperties" l with
           Some (Bool (loc, false)) -> Stack.push l2_orelse (UtFalse loc)
         | Some (Bool (_, true)) -> ()
         | Some (Assoc (loc, _) as j) -> begin
             match conv_type_l j with
               ([], _) -> Fmt.(raise_failwithf loc "additionalProperties yielded no type-constraints")
             | _ ->
               Stack.push l2_orelse (conv_type0 j)
           end
         | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: additionalProperties did not have bool or type payload")
         | None -> ()
        ) ;
        (match assoc_opt "additionalItems" l with
           Some (Bool (loc, false)) -> Stack.push l2_orelse (UtFalse loc)
         | Some (Bool (_, true)) -> ()
         | Some (Assoc (loc, _) as j) -> begin
             match conv_type_l j with
               ([], _) -> Fmt.(raise_failwithf loc "additionalItems yielded no type-constraints")
             | _ ->
               Stack.push l2_orelse (conv_type0 j)
           end
         | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: additionalItems did not have bool or type payload")
         | None -> ()
        ) ;
        (match assoc_opt "unevaluatedProperties" l with
           Some (Bool (loc, false)) -> Stack.push l2_orelse (UtFalse loc)
         | Some (Assoc (loc, _) as j) -> begin
             match conv_type_l j with
               ([], _) -> Fmt.(raise_failwithf loc "unevaluatedProperties yielded no type-constraints")
             | _ ->
               Stack.push l2_orelse (conv_type0 j)
           end
         | Some v -> Fmt.(raise_failwithf (LJ.to_loc v) "conv_type: unevaluatedProperties did not have bool or type payload")
         | None -> ()
        ) ;
        (l1,(!l2_additional, !l2_orelse))
      end

    | j -> Fmt.(raise_failwithf (LJ.to_loc j) "conv_type: expected an object")

  and wrap_seal j ut (patterns, orelses) =
    let loc = LJ.to_loc j in
    let has_orelse, orelse = match orelses with
        [] -> false, UtFalse loc
      | [ut] -> true, ut
      | _ -> Fmt.(raise_failwithf loc "conv_type: type had more than one Orelse") in
    let addSealed = patterns <> [] || has_orelse in
      if addSealed then
        Seal(loc, ut, List.stable_sort Stdlib.compare patterns, orelse)
      else ut

  and conv_type0 ?(allow_empty=false) t =
    let loc = LJ.to_loc t in
    match conv_type_l t with
      ([],([],[])) ->
      if allow_empty then begin
        Fmt.(pf stderr "%s:WARNING: conv_type: conversion produced no result"
               (Ploc.string_of_location loc)
            ) ;
        UtTrue loc
      end
      else
        Fmt.(raise_failwithf loc "conv_type: conversion produced no result")
    | ([],l2) ->
      Fmt.(pf stderr "%s:WARNING: conv_type: conversion produced no type, but sealing directives"
             (Ploc.string_of_location loc)
          ) ;
      wrap_seal t (UtFalse loc) l2

    | (l,([], [])) -> andList loc l
    | (l1,l2) ->
      let ut = andList loc l1 in
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
        let anno = if isSealed then Some(AN.mk isSealed) else None in
        drec ((tname, anno, def)::acc)
    in
    drec []

let recognize_definition t = match t with
  Assoc (_, l) ->
  l |> List.exists (function (k, v) ->
      List.mem k known_useful_keys
    )
  | _ -> false

let recognize_definitions cc t = match t with
    Assoc (loc, l) -> begin match (assoc_opt "definitions" l, assoc_opt "$defs" l, assoc_opt "defs" l ) with
        (Some (Assoc (loc, l)), None, None) ->
        l |> List.iter (function (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref loc (Printf.sprintf "#/definitions/%s" k))
          )
      | (None, Some (Assoc (loc, l)), None) ->
        l |> List.iter (fun (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref loc (Printf.sprintf "#/$defs/%s" k))
          )
      | (None, None, Some (Assoc (loc, l))) ->
        l |> List.iter (fun (k, v) ->
            if recognize_definition v then
              ignore(lookup_ref loc (Printf.sprintf "#/defs/%s" k))
          )
      | (None, None, None) -> ()
      | _ -> Fmt.(raise_failwithf loc "recognize_definitions: malformed")
    end
  | _ -> ()

let conv_schema cc t =
  let loc = LJ.to_loc t in
  reset cc t ;
  recognize_definitions cc t ;
  let t = top_conv_type t in
  let defs = conv_definitions() in
  let defs = List.stable_sort Stdlib.compare defs in
  let l = List.map (fun (id, mid) -> StImport(loc, id, mid)) !imports in
  let l = if defs <> [] then
      l@[
        StModuleBinding(loc, ID.of_string "DEFINITIONS", MeStruct (loc, [StTypes(loc, true, defs)]))
      ; StOpen(loc, REL (ID.of_string "DEFINITIONS"), None)
      ]
    else l in
  let isSealed = match t with Seal _ -> true | _ -> false in
  let anno = if isSealed then Some (AN.mk isSealed) else None in
  l@[StTypes(loc, false, [(ID.of_string "t", anno, t)])]

let convert_file ?(with_predefined=false) cc s =
  let open Fpath in
  let stl =
    if has_ext "json" (v s) then
      let j = parse_file parse_json_eoi s in
      conv_schema (CC.with_filename cc s) j
    else Fmt.(failwithf "convert_file: format not recognized: %s" s) in
  if with_predefined then
    let loc = match stl with [] -> Ploc.dummy | h::t -> loc_of_struct_item h in
  [StImport(loc, "lib/predefined.utj", ID.of_string "Predefined")]@stl
  else stl
