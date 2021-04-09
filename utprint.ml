open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;
open Utlexing ;


value plist_with sep f sh pc l =
  let l = List.map (fun s -> (s, sep)) l in
  pprintf pc "%p" (Prtools.plist f sh) l
;

value pr_utype = Eprinter.make "utype";
value print_utype = Eprinter.apply pr_utype;

value pr_module_expr = Eprinter.make "module_expr";
value print_module_expr = Eprinter.apply pr_module_expr;
value pr_module_type = Eprinter.make "module_type";
value print_module_type = Eprinter.apply pr_module_type;

value pr_struct_item = Eprinter.make "struct_item";
value print_struct_item = Eprinter.apply pr_struct_item;
value pr_sig_item = Eprinter.make "sig_item";
value print_sig_item = Eprinter.apply pr_sig_item;
value pr_base_type = Eprinter.make "base_type";
value print_base_type = Eprinter.apply pr_base_type;
value pr_atomic = Eprinter.make "atomic";
value print_atomic = Eprinter.apply pr_atomic;

value qstring pc s =
  pprintf pc "\"%s\"" s
;
value string pc s = pprintf pc "%s" s ;

value print_size_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.exclusive then "(" else "[")
    (string_of_int lo.it)
    (match hi.it with [ None -> "max" | Some n -> string_of_int n ])
    (if hi.exclusive then ")" else "]")
;

value print_range_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.exclusive then "(" else "[")
    (match lo.it with [ None -> "min" | Some n -> string_of_float n ])
    (match hi.it with [ None -> "max" | Some n -> string_of_float n ])
    (if hi.exclusive then ")" else "]")
;

value print_id pc id = pprintf pc "%s" id ;

value print_json pc j = pprintf pc "%s" (Yojson.Basic.to_string j) ;

value print_module_path pc l = plist_with "." string 0 pc l ;

EXTEND_PRINTER
  pr_module_expr: [ [
    MeStruct l -> pprintf pc "struct@;%p@;end" (plist_with "" print_struct_item 2) l
  | MeFunctorApp me1 me2 ->  pprintf pc "%p(%p)" print_module_expr me1 print_module_expr me2
  | MePath p -> print_module_path pc p
  | MeFunctor (id, mty) me ->  pprintf pc "functor (%s:%p) -> %p" id print_module_type mty print_module_expr me
  ] ] ;
  pr_module_type: [ [
    MtSig l -> pprintf pc "sig@;%p@;end" (plist_with "" print_sig_item 2) l
  | MtFunctorType (id, mty1) mty2 -> pprintf pc "functor (%s:%p) -> %p" id print_module_type mty1 print_module_type mty2
  | MtPath p -> print_module_path pc p
  ] ] ;
  pr_sig_item: [ [
    SiType s -> pprintf pc "%s;" s
  | SiModuleBinding s mty -> 
    pprintf pc "module %s : %p;" s print_module_type mty
  | SiModuleType s mty ->
    pprintf pc "module type %s = %p;" s print_module_type mty
  | SiInclude p ->
    pprintf pc "include %p;" print_module_path p
  ] ] ;
  pr_struct_item: [ [
    StTypes recflag l ->
    pprintf pc "type%s %p;" (if recflag then " rec" else " nonrec")
      (Prtools.vlist2
         (fun pc (s,t) -> pprintf pc "%s = %p" s print_utype t)
         (fun pc (s,t) -> pprintf pc "and %s = %p" s print_utype t)
      ) l
  | StModuleBinding id mexp ->
    pprintf pc "module %s = %p;" id print_module_expr mexp
  | StModuleType id mty ->
    pprintf pc "module type %s = %p;" id print_module_type mty
  | StImport url id ->
    pprintf pc "import %p as %s;" qstring url id
  | StLocal l1 l2 ->
    pprintf pc "local %p in %p end;" 
      (plist_with "" print_struct_item 0) l1
      (plist_with "" print_struct_item 0) l2
  | StOpen p ->
    pprintf pc "open %p;" print_module_path p
  | StInclude p ->
    pprintf pc "include %p;" print_module_path p
  ] ] ;

  pr_utype:
    [ "||"
      [ Or x y -> pprintf pc "%p || %p" next x curr y ]
    | "xor"
      [ Xor x y -> pprintf pc "%p xor %p" next x curr y ]
    | "&&"
      [ And x y -> pprintf pc "%p && %p" next x curr y ]
    | "=>"
      [ Impl x y -> pprintf pc "%p => %p" next x curr y ]
    | "not"
      [ Not x -> pprintf pc "not %p" next x ]
    | "simple"
      [ Simple x -> pprintf pc "%p" print_base_type x
      | Atomic l -> pprintf pc "[@[<2>@;%p@;]@]" (Prtools.vlist print_atomic) l
      | Ref [] id -> pprintf pc "%p" print_id id
      | Ref l id -> pprintf pc "%p.%p" (plist_with "." print_id 0) l print_id id
      | x -> pprintf pc "(%p)" print_utype x
      ]
    ] ;
  pr_base_type:
    [ [ JNull -> pprintf pc "null"
      | JString -> pprintf pc "string"
      | JBool -> pprintf pc "boolean"
      | JNumber -> pprintf pc "number"
      | JArray -> pprintf pc "array"
      | JObject -> pprintf pc "object"
    ] ] ;
  pr_atomic:
    [ [ Field s t -> pprintf pc "%p: %p;" qstring s print_utype t
      | FieldRE re t -> pprintf pc "/%s/ : %p;" (Escape.regexp re) print_utype t
      | FieldRequired l -> pprintf pc "required %p;" (plist_with ", " qstring 0) l
      | ArrayOf t -> pprintf pc "of %p;" print_utype t
      | ArrayTuple l -> pprintf pc "%p;" (plist_with " * " print_utype 0) l
      | ArrayUnique -> pprintf pc "unique;"
      | ArrayIndex n t -> pprintf pc "%d: %p" n print_utype t
      | Size sc -> pprintf pc "size %p;" print_size_constraint sc
      | StringRE re -> pprintf pc "/%s/" (Escape.regexp re)
      | NumberBound rc -> pprintf pc "bounds %p;" print_range_constraint rc
      | Sealed True -> pprintf pc "sealed;"
      | Sealed False -> pprintf pc "unsealed;"
      | OrElse t -> pprintf pc "orelse %p;" print_utype t
      | Enum l -> pprintf pc "enum %p;" (plist_with "," print_json 0) l
      | Default j -> pprintf pc "default %p;" print_json j
      | Format s -> pprintf pc "format %p;" qstring s
      | PropertyNames t -> pprintf pc "propertyNames %p;" print_utype t
      | ContentMediaType s -> pprintf pc "contentMediaType %p;" qstring s
      | ContentEncoding s -> pprintf pc "contentEncoding %p;" qstring s
    ] ] ;

END;
