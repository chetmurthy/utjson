
module ToAG = struct

module SRC = Utypes
module DST = Utypes2.REC.AT

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
and atomic_utype_t_node = [%import: Utypes.atomic_utype_t]
and atomic_utype_t = atomic_utype_t_node

and utype_t_node = [%import: Utypes.utype_t
  [@with ID.t := id_t]
]
and utype_t = utype_t_node

and struct_item_t_node = [%import: Utypes.struct_item_t
  [@with ID.t := id_t]
]
and struct_item_t = struct_item_t_node

and module_path_t = [%import: Utypes.module_path_t
  [@with ID.t := id_t]
]

and ref_t = [%import: Utypes.ref_t
  [@with ID.t := id_t]
]

and structure_node = [%import: Utypes.structure]
and structure = structure_node

and module_expr_t_node = [%import: Utypes.module_expr_t
  [@with ID.t := id_t]
]
and module_expr_t = module_expr_t_node

and module_type_t_node = [%import: Utypes.module_type_t
  [@with ID.t := id_t]
]
and module_type_t = module_type_t_node

and signature_node = [%import: Utypes.signature]
and signature = signature_node

and sig_item_t_node = [%import: Utypes.sig_item_t
  [@with ID.t := id_t]
]
and sig_item_t = sig_item_t_node
(*
and top_binding_t = [%import: Utypes.top_binding_t]
and top_bindings = [%import: Utypes.top_bindings]
*)
[@@deriving migrate
    { optional = true
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = true
    ; dispatchers = {
        migrate_utype_t = {
          srctype = [%typ: utype_t]
        ; dsttype = [%typ: DST.utype_t]
        ; code = (fun __dt__ x ->
            DST.make_utype_t (__dt__.migrate_utype_t_node __dt__ x)
          )
        }
      ; migrate_utype_t_node = {
          srctype = [%typ: utype_t_node]
        ; dsttype = [%typ: DST.utype_t_node]
        }
      ; migrate_module_expr_t = {
          srctype = [%typ: module_expr_t]
        ; dsttype = [%typ: DST.module_expr_t]
        ; code = (fun __dt__ x ->
            DST.make_module_expr_t (__dt__.migrate_module_expr_t_node __dt__ x)
          )
        }
      ; migrate_module_expr_t_node = {
          srctype = [%typ: module_expr_t_node]
        ; dsttype = [%typ: DST.module_expr_t_node]
        ; custom_branches_code = function
            | MeFunctor (loc, (id, mty), me) ->
              DST.MeFunctor
                (__dt__.migrate_loc __dt__ loc,
                 __dt__.migrate_id_t __dt__ id,
                 __dt__.migrate_module_type_t __dt__ mty,
                 __dt__.migrate_module_expr_t __dt__ me)
        }
      ; migrate_module_type_t = {
          srctype = [%typ: module_type_t]
        ; dsttype = [%typ: DST.module_type_t]
        ; code = (fun __dt__ x ->
            DST.make_module_type_t (__dt__.migrate_module_type_t_node __dt__ x)
          )
        }
      ; migrate_module_type_t_node = {
          srctype = [%typ: module_type_t_node]
        ; dsttype = [%typ: DST.module_type_t_node]
        ; custom_branches_code = function
            | MtFunctorType (loc, (id, mty1), mty2) ->
              DST.MtFunctorType
                (__dt__.migrate_loc __dt__ loc,
                 __dt__.migrate_id_t __dt__ id,
                 __dt__.migrate_module_type_t __dt__ mty1,
                 __dt__.migrate_module_type_t __dt__ mty2)
        }

      ; migrate_struct_item_t = {
          srctype = [%typ: struct_item_t]
        ; dsttype = [%typ: DST.struct_item_t]
        ; code = (fun __dt__ x ->
            DST.make_struct_item_t (__dt__.migrate_struct_item_t_node __dt__ x)
          )
        }
      ; migrate_struct_item_t_node = {
          srctype = [%typ: struct_item_t_node]
        ; dsttype = [%typ: DST.struct_item_t_node]
        ; custom_branches_code = function
            | StOpen (loc, mp, _) ->
              let open DST in
              StOpen
                (__dt__.migrate_loc __dt__ loc,
                 __dt__.migrate_module_path_t __dt__ mp)
            | StInclude (loc, mp, _) ->
              let open DST in
              StInclude
                (__dt__.migrate_loc __dt__ loc,
                 __dt__.migrate_module_path_t __dt__ mp)

        }
      ; migrate_structure = {
          srctype = [%typ: structure]
        ; dsttype = [%typ: DST.structure]
        ; code = (fun __dt__ x ->
            DST.make_structure (__dt__.migrate_structure_node __dt__ x)
          )
        }
      ; migrate_structure_node = {
          srctype = [%typ: structure_node]
        ; dsttype = [%typ: DST.structure_node]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> DST.StructureNil
              | h::t -> DST.(StructureCons (__dt__.migrate_struct_item_t __dt__ h, make_structure (mrec t)))
            in mrec l)
        }

      ; migrate_signature = {
          srctype = [%typ: signature]
        ; dsttype = [%typ: DST.signature]
        ; code = (fun __dt__ x ->
            DST.make_signature (__dt__.migrate_signature_node __dt__ x)
          )
        }
      ; migrate_signature_node = {
          srctype = [%typ: signature_node]
        ; dsttype = [%typ: DST.signature_node]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> DST.SignatureNil
              | h::t -> DST.(SignatureCons (__dt__.migrate_sig_item_t __dt__ h, make_signature (mrec t)))
            in mrec l)
        }

      ; migrate_sig_item_t = {
          srctype = [%typ: sig_item_t]
        ; dsttype = [%typ: DST.sig_item_t]
        ; code = (fun __dt__ x ->
            DST.make_sig_item_t (__dt__.migrate_sig_item_t_node __dt__ x)
          )
        }
      ; migrate_sig_item_t_node = {
          srctype = [%typ: sig_item_t_node]
        ; dsttype = [%typ: DST.sig_item_t_node]
        }
      ; migrate_atomic_utype_t = {
          srctype = [%typ: atomic_utype_t]
        ; dsttype = [%typ: DST.atomic_utype_t]
        ; code = (fun __dt__ x ->
            DST.make_atomic_utype_t (__dt__.migrate_atomic_utype_t_node __dt__ x)
          )
        }
      ; migrate_atomic_utype_t_node = {
          srctype = [%typ: atomic_utype_t_node]
        ; dsttype = [%typ: DST.atomic_utype_t_node]
(*
        ; custom_branches_code = function
            | ArrayTuple (v_0, v_1) ->
              let open DST in
              ArrayTuple
                ((fun __dt__ -> __dt__.migrate_loc __dt__) __dt__ v_0,
                 (fun __dt__ ->
                    __dt__.migrate_list
                      (fun __dt__ -> __dt__.migrate_utype_t __dt__) __dt__)
                   __dt__ v_1)
*)
        }
      ; migrate_utype_t_option = {
          srctype = [%typ: utype_t option]
        ; dsttype = [%typ: DST.utype_t_option]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
              | None -> DST.(make_utype_t_option UtOption_None)
              | Some ut -> DST.(make_utype_t_option (UtOption_Some (__dt__.migrate_utype_t __dt__ ut)))
            in mrec l)
        }
      ; migrate_utype_t_list = {
          srctype = [%typ: utype_t list]
        ; dsttype = [%typ: DST.utype_t_list]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> failwith "utype_t_list: must be non-nil"
              | [t] -> DST.(make_utype_t_list (UtSingleton (__dt__.migrate_utype_t __dt__ t)))
              | h::t -> DST.(make_utype_t_list (UtCons (__dt__.migrate_utype_t __dt__ h, mrec t)))
            in mrec l)
        }
      ; migrate_atomic_utype_t_list = {
          srctype = [%typ: atomic_utype_t list]
        ; dsttype = [%typ: DST.atomic_utype_t_list]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> failwith "atomic_utype_t_list: must be non-nil"
              | [t] -> DST.(make_atomic_utype_t_list (AtomicSingleton (__dt__.migrate_atomic_utype_t __dt__ t)))
              | h::t -> DST.(make_atomic_utype_t_list (AtomicCons (__dt__.migrate_atomic_utype_t __dt__ h, mrec t)))
            in mrec l)
        }
      ; migrate_utype_t_binding_list = {
          srctype = [%typ: (id_t * bool * utype_t) list]
        ; dsttype = [%typ: DST.utype_t_binding_list]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> failwith "utype_t_binding_list: must be non-nil"
              | [(id,b,t)] -> DST.(make_utype_t_binding_list (UTTypeBindSingleton (id, b, __dt__.migrate_utype_t __dt__ t)))
              | (id, b, h)::t -> DST.(make_utype_t_binding_list (UTTypeBindCons (id, b, __dt__.migrate_utype_t __dt__ h, mrec t)))
            in mrec l)
        }
      ; migrate_re_utype_t_list = {
          srctype = [%typ: (string * utype_t) list]
        ; dsttype = [%typ: DST.re_utype_t_list]
        ; code =  (fun __dt__ l ->
            let rec mrec = function
                [] -> failwith "re_utype_t_list: must be non-nil"
              | [(re,t)] -> DST.(make_re_utype_t_list (REUT_Singleton (re, __dt__.migrate_utype_t __dt__ t)))
              | (re, h)::t -> DST.(make_re_utype_t_list (REUT_Cons (re, __dt__.migrate_utype_t __dt__ h, mrec t)))
            in mrec l)
        }
(*
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
*)
      ; migrate_string_list = {
          srctype = [%typ: string list]
        ; dsttype = [%typ: string list]
        ; code = fun __dt__ l -> _migrate_list (fun _ x -> x) __dt__ l
        }
      ; migrate_json = {
          srctype = [%typ: Yojson.Basic.t]
        ; dsttype = [%typ: Yojson.Basic.t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_json_list = {
          srctype = [%typ: Yojson.Basic.t list]
        ; dsttype = [%typ: Yojson.Basic.t list]
        ; code = fun __dt__ x -> x
        }
(*
      ; migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
*)
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: Utypes.loc]
        ; code = fun __dt__ x -> x
        }
      ; migrate_base_type_t = {
          srctype = [%typ: base_type_t]
        ; dsttype = [%typ: Utypes.base_type_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_size_constraint_t = {
          srctype = [%typ: size_constraint_t]
        ; dsttype = [%typ: Utypes.size_constraint_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_range_constraint_t = {
          srctype = [%typ: range_constraint_t]
        ; dsttype = [%typ: Utypes.range_constraint_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_ref_t = {
          srctype = [%typ: ref_t]
        ; dsttype = [%typ: Utypes.ref_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_id_t = {
          srctype = [%typ: id_t]
        ; dsttype = [%typ: id_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_module_path_t = {
          srctype = [%typ: module_path_t]
        ; dsttype = [%typ: Utypes.module_path_t]
        ; code = fun __dt__ x -> x
        }
      }
    }]
end

module FromAG = struct

module SRC = Utypes2.REC.AT
module DST = Utypes

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  l |> List.rev_map (subrw0 __dt__) |> List.rev

[%%import: Utypes2.REC.AT.atomic_utype_t
  [@add type loc = [%import: Utypes.loc]
         and yojson = Yojson.Basic.t
         and yojson_list = Yojson.Basic.t list
         and string_list = string list
         and range_constraint_t = [%import: Utypes.range_constraint_t
             [@with Bound.t := bound_t]
         ]
         and size_constraint_t = [%import: Utypes.size_constraint_t
             [@with Bound.t := bound_t]
         ]
         and id_t = Utypes.ID.t
         and module_path_t = [%import: Utypes.module_path_t
             [@with ID.t := id_t]
         ]
         and ref_t = [%import: Utypes.ref_t
             [@with ID.t := id_t]
         ]
         and base_type_t = [%import: Utypes.base_type_t]
         and 'a bound_t = [%import: 'a Utypes.Bound.t]
  ]
]
(*
and top_binding_t = [%import: Utypes.top_binding_t]
and top_bindings = [%import: Utypes.top_bindings]
*)
[@@deriving migrate
    { optional = true
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = true
    ; dispatchers = {
        migrate_utype_t = {
          srctype = [%typ: utype_t]
        ; dsttype = [%typ: DST.utype_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_utype_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_utype_t_node = {
          srctype = [%typ: utype_t_node]
        ; dsttype = [%typ: DST.utype_t]
        }
      ; migrate_atomic_utype_t = {
          srctype = [%typ: atomic_utype_t]
        ; dsttype = [%typ: DST.atomic_utype_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_atomic_utype_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_atomic_utype_t_node = {
          srctype = [%typ: atomic_utype_t_node]
        ; dsttype = [%typ: DST.atomic_utype_t]
        }

      ; migrate_module_expr_t = {
          srctype = [%typ: module_expr_t]
        ; dsttype = [%typ: DST.module_expr_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_module_expr_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_module_expr_t_node = {
          srctype = [%typ: module_expr_t_node]
        ; dsttype = [%typ: DST.module_expr_t]
        ; custom_branches_code = function
            | MeFunctor (loc, id, mty, me) ->
              let open DST in
              MeFunctor
                (__dt__.migrate_loc __dt__ loc,
                 (__dt__.migrate_id_t __dt__ id,
                  __dt__.migrate_module_type_t __dt__ mty),
                 __dt__.migrate_module_expr_t __dt__ me)
        }


      ; migrate_module_type_t = {
          srctype = [%typ: module_type_t]
        ; dsttype = [%typ: DST.module_type_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_module_type_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_module_type_t_node = {
          srctype = [%typ: module_type_t_node]
        ; dsttype = [%typ: DST.module_type_t]
        ; custom_branches_code = function
            | MtFunctorType (loc, id, mty1, mty2) ->
              let open DST in
              MtFunctorType
                (__dt__.migrate_loc __dt__ loc,
                 (__dt__.migrate_id_t __dt__ id,
                  __dt__.migrate_module_type_t __dt__ mty1),
                 __dt__.migrate_module_type_t __dt__ mty2)
        }


      ; migrate_struct_item_t = {
          srctype = [%typ: struct_item_t]
        ; dsttype = [%typ: DST.struct_item_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_struct_item_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_struct_item_t_node = {
          srctype = [%typ: struct_item_t_node]
        ; dsttype = [%typ: DST.struct_item_t]
        ; custom_branches_code = function
            | StInclude (v_0, v_1) ->
              let open DST in
              StInclude
                (__dt__.migrate_loc __dt__ v_0,
                 __dt__.migrate_module_path_t __dt__ v_1,
                 None)
            | StOpen (v_0, v_1) ->
              let open DST in
              StOpen
                (__dt__.migrate_loc __dt__ v_0,
                 __dt__.migrate_module_path_t __dt__ v_1,
                 None)
        }


      ; migrate_sig_item_t = {
          srctype = [%typ: sig_item_t]
        ; dsttype = [%typ: DST.sig_item_t]
        ; code = (fun __dt__ x ->
            __dt__.migrate_sig_item_t_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_sig_item_t_node = {
          srctype = [%typ: sig_item_t_node]
        ; dsttype = [%typ: DST.sig_item_t]
        }

      ; migrate_structure = {
          srctype = [%typ: structure]
        ; dsttype = [%typ: DST.structure]
        ; code = (fun __dt__ x ->
            __dt__.migrate_structure_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_structure_node = {
          srctype = [%typ: structure_node]
        ; dsttype = [%typ: DST.structure]
        ; code = fun __dt__ l ->
            let rec mrec = function
                StructureCons (v_0, v_1) ->
                (__dt__.migrate_struct_item_t __dt__ v_0)::
                (__dt__.migrate_structure __dt__ v_1)
              | StructureNil -> []
            in mrec l
        }
      ; migrate_signature = {
          srctype = [%typ: signature]
        ; dsttype = [%typ: DST.signature]
        ; code = (fun __dt__ x ->
            __dt__.migrate_signature_node __dt__ x.Pa_ppx_ag_runtime.Attributes.node
          )
        }
      ; migrate_signature_node = {
          srctype = [%typ: signature_node]
        ; dsttype = [%typ: DST.signature]
        ; code = fun __dt__ l ->
            let rec mrec = function
                SignatureCons (v_0, v_1) ->
                (__dt__.migrate_sig_item_t __dt__ v_0)::
                (__dt__.migrate_signature __dt__ v_1)
              | SignatureNil -> []
            in mrec l
        }
      ; migrate_utype_t_list = {
          srctype = [%typ: utype_t_list]
        ; dsttype = [%typ: DST.utype_t list]
        ; code = (fun __dt__ x ->
            let rec mrec x = match x.Pa_ppx_ag_runtime.Attributes.node with
                  UtSingleton x -> [__dt__.migrate_utype_t __dt__ x]
                | UtCons(h,t) -> (__dt__.migrate_utype_t __dt__ h)::(mrec t)
            in mrec x                                        
          )
        }
      ; migrate_utype_t_option = {
          srctype = [%typ: utype_t_option]
        ; dsttype = [%typ: DST.utype_t option]
        ; code = (fun __dt__ x ->
            let rec mrec x = match x.Pa_ppx_ag_runtime.Attributes.node with
                  UtOption_None -> None
                | UtOption_Some ut -> Some(__dt__.migrate_utype_t __dt__ ut)
            in mrec x                                        
          )
        }
      ; migrate_utype_t_binding_list = {
          srctype = [%typ: utype_t_binding_list]
        ; dsttype = [%typ: (id_t * bool * DST.utype_t) list]
        ; code = (fun __dt__ x ->
            let rec mrec x = match x.Pa_ppx_ag_runtime.Attributes.node with
                  UTTypeBindSingleton (id,b,x) -> [(id,b,__dt__.migrate_utype_t __dt__ x)]
                | UTTypeBindCons(id, b, h,t) -> (id, b, __dt__.migrate_utype_t __dt__ h)::(mrec t)
            in mrec x                                        
          )
        }
      ; migrate_re_utype_t_list = {
          srctype = [%typ: re_utype_t_list]
        ; dsttype = [%typ: (string * DST.utype_t) list]
        ; code = (fun __dt__ x ->
            let rec mrec x = match x.Pa_ppx_ag_runtime.Attributes.node with
                REUT_Singleton (s,x) -> [(s,__dt__.migrate_utype_t __dt__ x)]
              | REUT_Cons(s, h,t) -> (s, __dt__.migrate_utype_t __dt__ h)::(mrec t)
            in mrec x                                        
          )
        }
      ; migrate_atomic_utype_t_list = {
          srctype = [%typ: atomic_utype_t_list]
        ; dsttype = [%typ: DST.atomic_utype_t list]
        ; code = (fun __dt__ x ->
            let rec mrec x = match x.Pa_ppx_ag_runtime.Attributes.node with
                  AtomicSingleton x -> [__dt__.migrate_atomic_utype_t __dt__ x]
                | AtomicCons(h,t) -> (__dt__.migrate_atomic_utype_t __dt__ h)::(mrec t)
            in mrec x                                        
          )
        }
      ; migrate_string_list = {
          srctype = [%typ: string_list]
        ; dsttype = [%typ: string list]
        ; code = fun __dt__ l -> _migrate_list (fun _ x -> x) __dt__ l
        }
      ; migrate_json = {
          srctype = [%typ: yojson]
        ; dsttype = [%typ: Yojson.Basic.t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_json_list = {
          srctype = [%typ: yojson_list]
        ; dsttype = [%typ: Yojson.Basic.t list]
        ; code = fun __dt__ x -> x
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: Utypes.loc]
        ; code = fun __dt__ x -> x
        }
      ; migrate_base_type_t = {
          srctype = [%typ: base_type_t]
        ; dsttype = [%typ: Utypes.base_type_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_size_constraint_t = {
          srctype = [%typ: size_constraint_t]
        ; dsttype = [%typ: Utypes.size_constraint_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_range_constraint_t = {
          srctype = [%typ: range_constraint_t]
        ; dsttype = [%typ: Utypes.range_constraint_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_ref_t = {
          srctype = [%typ: ref_t]
        ; dsttype = [%typ: Utypes.ref_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_id_t = {
          srctype = [%typ: id_t]
        ; dsttype = [%typ: id_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_module_path_t = {
          srctype = [%typ: module_path_t]
        ; dsttype = [%typ: Utypes.module_path_t]
        ; code = fun __dt__ x -> x
        }
      }
    }]
end
