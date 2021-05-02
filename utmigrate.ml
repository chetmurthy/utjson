
module Self = struct

module SRC = Utypes
module DST = Utypes

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  l |> List.rev_map (subrw0 __dt__) |> List.rev

type loc = [%import: Utypes.loc]
and base_type_t = [%import: Utypes.base_type_t]
and 'a bound_t = [%import: 'a Utypes.Bound.t]
and id_t = [%import: Utypes.ID.t]
and size_constraint_t = [%import: Utypes.size_constraint_t
  [@with Bound.t := bound_t]
]
and range_constraint_t = [%import: Utypes.range_constraint_t
  [@with Bound.t := bound_t]
]
and atomic_utype_t = [%import: Utypes.atomic_utype_t]
and utype_t = [%import: Utypes.utype_t
  [@with ID.t := id_t]
]
and annotation_t = [%import: Utypes.AN.t
  [@with Utypes.AN.kind := kind]
]
and annotation_opt = annotation_t option
and kind = [%import: Utypes.AN.kind]
and struct_item_t = [%import: Utypes.struct_item_t
  [@with ID.t := id_t]
  [@with AN.t := annotation_t]
]
and module_path_t = [%import: Utypes.module_path_t
  [@with ID.t := id_t]
]
and ref_t = [%import: Utypes.ref_t
  [@with ID.t := id_t]
]
and structure = [%import: Utypes.structure]
and module_expr_t = [%import: Utypes.module_expr_t
  [@with ID.t := id_t]
]
and module_type_t = [%import: Utypes.module_type_t
  [@with ID.t := id_t]
]
and signature = [%import: Utypes.signature]
and sig_item_t = [%import: Utypes.sig_item_t
  [@with ID.t := id_t]
  [@with AN.t := annotation_t]
]

and top_binding_t = [%import: Utypes.top_binding_t
  [@with AN.t := annotation_t]
]
and top_bindings = [%import: Utypes.top_bindings]
[@@deriving migrate
    { optional = true
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = true
    ; default_dispatchers = [
        {
          srcmod = SRC
        ; dstmod = DST
        ; types = [
            base_type_t
          ; bound_t
          ; id_t
          ; size_constraint_t
          ; range_constraint_t
          ; atomic_utype_t
          ; utype_t
          ; struct_item_t
          ; module_path_t
          ; structure
          ; module_expr_t
          ; module_type_t
          ; signature
          ; sig_item_t
          ; ref_t
          ; top_binding_t
          ; top_bindings
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_json = {
          srctype = [%typ: Yojson.Basic.t]
        ; dsttype = [%typ: Yojson.Basic.t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: Utypes.loc]
        ; code = fun __dt__ x -> x
        }
      ; migrate_kind = {
          srctype = [%typ: kind]
        ; dsttype = [%typ: Utypes.AN.kind]
        ; code = fun __dt__ x -> x
        }
      ; migrate_annotation = {
          srctype = [%typ: annotation_t]
        ; dsttype = [%typ: Utypes.AN.t]
        }
      ; migrate_annotation_opt = {
          srctype = [%typ: annotation_opt]
        ; dsttype = [%typ: Utypes.AN.t option]
        }
      }
    }]
end
