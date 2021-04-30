open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;
open Utlexing ;


value plist_with sep f sh pc l =
  let l = List.map (fun s -> (s, sep)) l in
  pprintf pc "%p" (Prtools.plist f sh) l
;

value qstring pc s =
  pprintf pc "\"%s\"" s
;
value string pc s = pprintf pc "%s" s ;

value string_of_jsonfloat f =
  let s = string_of_float f in
  if String.get s (String.length s - 1) = '.' then
    s^"0"
  else s
;

value rec print_utype pc x = pr_utype pc x
and print_module_expr pc x = pr_module_expr pc x
and print_module_type pc x = pr_module_type pc x
and print_struct_item pc x = pr_struct_item pc x
and print_sig_item pc x = pr_sig_item pc x
and print_base_type pc x = pr_base_type pc x
and print_atomic pc x = pr_atomic pc x
and print_structure pc l = plist_with "" print_struct_item 2 pc l 
and print_signature pc l = plist_with "" print_sig_item 2 pc l 

and print_size_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.exclusive then "(" else "[")
    (string_of_int lo.it)
    (match hi.it with [ None -> "max" | Some n -> string_of_int n ])
    (if hi.exclusive then ")" else "]")

and print_range_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.exclusive then "(" else "[")
    (match lo.it with [ None -> "min" | Some n -> string_of_jsonfloat n ])
    (match hi.it with [ None -> "max" | Some n -> string_of_jsonfloat n ])
    (if hi.exclusive then ")" else "]")


and print_id pc id = pprintf pc "%s" (ID.to_string id)

and print_json pc j = pprintf pc "%s" (Yojson.Basic.to_string j)

and print_module_path pc = fun [
      REL h -> pprintf pc "%s" (ID.to_string h)
    | TOP h -> pprintf pc ".%s" (ID.to_string h)
    | DEREF p id -> pprintf pc "%p.%s" print_module_path p (ID.to_string id)
    ]

and pr_module_expr pc = fun [
    MeFunctorApp _ me1 me2 ->  pprintf pc "%p(%p)" print_module_expr me1 print_module_expr me2
  | MeCast _ me mt -> pprintf pc "%p : %p" print_module_expr me print_module_type mt
  | me -> pr_module_expr_simple pc me
  ]
and pr_module_expr_simple pc = fun [
    MeStruct _ l -> pprintf pc "struct@;%p@;end" print_structure l
  | MePath _ p -> print_module_path pc p
  | MeFunctor _ (id, mty) me ->  pprintf pc "functor (%s:%p) -> %p" (ID.to_string id) print_module_type mty print_module_expr me
  | me -> pprintf pc "(%p)" print_module_expr me
  ]

and pr_module_type pc = fun [
    MtSig _ l -> pprintf pc "sig@;%p@;end" print_signature l
  | MtFunctorType _ (id, mty1) mty2 -> pprintf pc "functor (%s:%p) -> %p" (ID.to_string id) print_module_type mty1 print_module_type mty2
  | MtPath _ (Some p, id) -> pprintf pc "%p.%s" print_module_path p (ID.to_string id)
  | MtPath _ (None, id) -> pprintf pc "%s" (ID.to_string id)
  ]
and pr_annotation pc = fun [
    AN.SEALED -> pprintf pc "[sealed]"
  | AN.UNSEALED l -> pprintf pc "[%p]" (plist_with "," pr_base_type 0) l
  ]
and pr_annotation_opt pc = fun [
    None -> pprintf pc ""
  | Some a -> pprintf pc "%p " pr_annotation a
  ]
and pr_sig_item pc = fun [
    SiType _ s anno -> pprintf pc "type %p %s;" pr_annotation anno (ID.to_string s)
  | SiModuleBinding _ s mty -> 
    pprintf pc "module %s : %p;" (ID.to_string s) print_module_type mty
  | SiModuleType _ s mty ->
    pprintf pc "module type %s = %p;" (ID.to_string s) print_module_type mty
  | SiInclude _ p ->
    pprintf pc "include %p;" print_module_path p
  ]
and pr_struct_item pc = fun [
    StTypes _ recflag l ->
    pprintf pc "type%s %p;" (if recflag then " rec" else " nonrec")
      (Prtools.vlist2
         (fun pc (s, anno_opt,t) -> pprintf pc "%p%s = %p" pr_annotation_opt anno_opt (ID.to_string s) print_utype t)
         (fun pc (s, anno_opt, t) -> pprintf pc "and %p%s = %p" pr_annotation_opt anno_opt (ID.to_string s) print_utype t)
      ) l
  | StModuleBinding _ id mexp ->
    pprintf pc "module %s = %p;" (ID.to_string id) print_module_expr mexp
  | StModuleType _ id mty ->
    pprintf pc "module type %s = %p;" (ID.to_string id) print_module_type mty
  | StImport _ url id ->
    pprintf pc "import %p as %s;" qstring url (ID.to_string id)
  | StLocal _ l1 l2 ->
    pprintf pc "local %p in %p end;" 
      (plist_with "" print_struct_item 0) l1
      (plist_with "" print_struct_item 0) l2
  | StOpen _ p None ->
    pprintf pc "open %p;" print_module_path p
  | StOpen _ p (Some ty) ->
    pprintf pc "open %p : %p;" print_module_path p print_module_type ty
  | StInclude _ p None ->
    pprintf pc "include %p;" print_module_path p
  | StInclude _ p (Some ty) ->
    pprintf pc "include %p : %p;" print_module_path p print_module_type ty
  ]
and pr_utype pc x = pr_utype_or pc x

and pr_utype_or pc = fun [
      Or _ x y -> pprintf pc "%p || %p" pr_utype_xor x pr_utype_or y
    | x -> pr_utype_xor pc x
    ]
and pr_utype_xor pc = fun [
      Xor _ x y -> pprintf pc "%p xor %p" pr_utype_and x pr_utype_xor y
    | x -> pr_utype_and pc x
    ]
and pr_utype_and pc = fun [
      And _ x y -> pprintf pc "%p && %p" pr_utype_impl x pr_utype_and y
    | x -> pr_utype_impl pc x
    ]
and pr_utype_impl pc = fun [
      Impl _ x y -> pprintf pc "%p => %p" pr_utype_not x pr_utype_impl y
    | x -> pr_utype_not pc x
    ]
and pr_utype_not pc = fun [
      Not _ x -> pprintf pc "not %p" pr_utype_seal x
    | x -> pr_utype_seal pc x
    ]
and pr_utype_seal pc = fun [
    Seal _ x [] (UtFalse _) -> pprintf pc "seal %p" pr_utype_simple x
  | Seal _ x l orelse -> pprintf pc "seal %p with %p" pr_utype_simple x pr_seal_extras (l,orelse)
  | x -> pr_utype_simple pc x
  ]
and pr_seal_extras pc = fun [
    ([(re,t)], UtFalse _) -> pprintf pc "/%s/ : %p" (Escape.regexp re) pr_utype_simple t
  | ([], t) ->  pprintf pc "%p" pr_utype_simple t
  | ([(re,t)::l],orelse) -> pprintf pc "/%s/ : %p, %p" (Escape.regexp re) pr_utype_simple t pr_seal_extras (l,orelse)
  ]
and pr_utype_simple pc = fun [
      Simple _ x -> pprintf pc "%p" print_base_type x
    | UtTrue _ -> pprintf pc "true"
    | UtFalse _ -> pprintf pc "false"
    | Atomic _ l -> pprintf pc "[@[<2>@;%p@;]@]" (Prtools.vlist print_atomic) l
    | Ref _ (None, id) -> pprintf pc "%p" print_id id
    | Ref _ (Some p, id) -> pprintf pc "%p.%p" print_module_path p print_id id
    | x -> pprintf pc "(%p)" print_utype x
    ]
and pr_base_type pc  = fun [
    JNull -> pprintf pc "null"
  | JString -> pprintf pc "string"
  | JBool -> pprintf pc "boolean"
  | JNumber -> pprintf pc "number"
  | JArray -> pprintf pc "array"
  | JObject -> pprintf pc "object"
  ]
and pr_atomic pc = fun [
    Field _ s t -> pprintf pc "%p: %p;" qstring s print_utype t
  | FieldRequired _ l -> pprintf pc "required %p;" (plist_with ", " qstring 0) l
  | ArrayOf _ t -> pprintf pc "of %p;" print_utype t
  | ArrayTuple _ l -> pprintf pc "%p;" (plist_with " * " print_utype 0) l
  | ArrayUnique _ -> pprintf pc "unique;"
  | ArrayIndex _ n t -> pprintf pc "%d: %p" n print_utype t
  | Size _ sc -> pprintf pc "size %p;" print_size_constraint sc
  | StringRE _ re -> pprintf pc "/%s/" (Escape.regexp re)
  | NumberBound _ rc -> pprintf pc "bounds %p;" print_range_constraint rc
  | Enum _ l -> pprintf pc "enum %p;" (plist_with "," print_json 0) l
  | Default _ j -> pprintf pc "default %p;" print_json j
  | Format _ s -> pprintf pc "format %p;" qstring s
  | PropertyNames _ t -> pprintf pc "propertyNames %p;" print_utype t
  | ContentMediaType _ s -> pprintf pc "contentMediaType %p;" qstring s
  | ContentEncoding _ s -> pprintf pc "contentEncoding %p;" qstring s
  | MultipleOf _ n ->  pprintf pc "multipleOf %s;" (string_of_jsonfloat n)
  ]

and pr_top_binding pc = fun [
      (_, (None, id), anno, ut) -> pprintf pc "%p %s = %p;" pr_annotation anno (ID.to_string id) print_utype ut
    | (_, (Some mp, id), anno, ut) -> pprintf pc "%p %p.%s = %p;" pr_annotation anno print_module_path mp (ID.to_string id) print_utype ut
    ]

and pr_top_bindings pc l =
    Prtools.vlist pr_top_binding pc l

and print_top_binding pc x = pr_top_binding pc x
and print_top_bindings pc x = pr_top_bindings pc x
;

