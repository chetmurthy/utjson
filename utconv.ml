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

let rec conv_type = function
    (`Assoc l) as j ->
    (match List.assoc "type" l with
       `String "null" -> [Simple JNull]
     | `String "string" -> [Simple JString]
     | `String "bool" -> [Simple JBool]
     | `String "number" -> [Simple JNumber]
     | `String "array" -> [Simple JArray]
     | `String "object" -> [Simple JObject]
     | v -> failwithf Fmt.(str "conv_type: malformed type member: %a" pp_json v)
     | exception Not_Found -> failwithf Fmt.(str "conv_type: lacks type member: %a" pp_json j)
    )@
    (match List.assoc "properties" l with
       `Assoc l ->
       Atomic (List.map (fun (k,v) -> Field k (conv_type v)) l)
     | v -> failwithf Fmt.(str "conv_type: malformed properties member: %a" pp_json v)
     | exception Not_found -> []
    )

  | j -> failwithf Fmt.(str "conv_type: %a" pp_json j)
