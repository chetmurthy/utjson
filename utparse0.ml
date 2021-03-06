open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;
open Ututil ;
open Utlexing ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]

] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value positions_to_loc ?{comments=""} (spos, epos) =
  let open Lexing in
  Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments
;

value convert_token (tok, pos) =
  let pos = positions_to_loc pos in
  let tok = match tok with [
    Spcl s -> ("",s)
  | Keyw s -> ("",s)
  | Uident s -> ("UIDENT",s)
  | Lident s -> ("LIDENT",s)
  | Integer n -> ("INT",n)
  | Float n -> ("FLOAT",n)
  | Regexp n -> ("REGEXP",Unescape.regexp n)
  | String s -> ("STRING", Unescape.jsonstring s)
  | EOF -> ("EOI","")
  ]
  in
  (tok, pos)
;

value lex_string s =
  let lexbuf = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec lexrec acc =
    match convert_token (rawtoken lexbuf) with [
      (("EOI",_),_) as t -> List.rev [t::acc]
    | t -> lexrec [t::acc]
    ] in lexrec []
;

value input_file = Plexing.input_file ;
value lexer_func_of_sedlex_located lexfun cs =
  let read1 () =
    try Some (Stream.next cs) with [ Stream.Failure -> None ] in
  let lexbuf = Sedlexing.Latin1.from_gen read1
  in
  let next_token_func () = convert_token (lexfun lexbuf) in do {
    Sedlexing.set_filename lexbuf input_file.val ;
    Plexing.make_stream_and_location next_token_func
  }
;

value lexer = lexer_func_of_sedlex_located rawtoken ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

value g = Grammar.gcreate lexer;

value (json : Grammar.Entry.e LJ.t) = Grammar.Entry.create g "json";
value (json_eoi : Grammar.Entry.e LJ.t) = Grammar.Entry.create g "json_eoi";

value (annotation : Grammar.Entry.e AN.t) = Grammar.Entry.create g "annotation";
value (annotation_eoi : Grammar.Entry.e AN.t) = Grammar.Entry.create g "annotation_eoi";

value (utype : Grammar.Entry.e utype_t) = Grammar.Entry.create g "utype";
value (utype_eoi : Grammar.Entry.e utype_t) = Grammar.Entry.create g "utype_eoi";

value (structure : Grammar.Entry.e structure) = Grammar.Entry.create g "structure";
value (structure_eoi : Grammar.Entry.e structure) = Grammar.Entry.create g "structure_eoi";

value (struct_item : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "struct_item";
value (struct_item_eoi : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "struct_item_eoi";

value (signature : Grammar.Entry.e signature) = Grammar.Entry.create g "signature";
value (signature_eoi : Grammar.Entry.e signature) = Grammar.Entry.create g "signature_eoi";

value (sig_item : Grammar.Entry.e (list sig_item_t)) = Grammar.Entry.create g "sig_item";
value (sig_item_eoi : Grammar.Entry.e (list sig_item_t)) = Grammar.Entry.create g "sig_item_eoi";

value (module_expr : Grammar.Entry.e module_expr_t) = Grammar.Entry.create g "module_expr";
value (module_expr_eoi : Grammar.Entry.e module_expr_t) = Grammar.Entry.create g "module_expr_eoi";

value (module_type : Grammar.Entry.e module_type_t) = Grammar.Entry.create g "module_type";
value (module_type_eoi : Grammar.Entry.e module_type_t) = Grammar.Entry.create g "module_type_eoi";

value (ref_t : Grammar.Entry.e ref_t) = Grammar.Entry.create g "ref_t";
value (ref_t_eoi : Grammar.Entry.e ref_t) = Grammar.Entry.create g "ref_t_eoi";

value (top_binding : Grammar.Entry.e top_binding_t) = Grammar.Entry.create g "top_binding";
value (top_binding_eoi : Grammar.Entry.e top_binding_t) = Grammar.Entry.create g "top_binding_eoi";

value (top_bindings : Grammar.Entry.e (list top_binding_t)) = Grammar.Entry.create g "top_bindings";
value (top_bindings_eoi : Grammar.Entry.e (list top_binding_t)) = Grammar.Entry.create g "top_bindings_eoi";

EXTEND
  GLOBAL:
    json json_eoi
    annotation annotation_eoi
    utype utype_eoi
    structure structure_eoi
    struct_item struct_item_eoi
    signature signature_eoi
    sig_item sig_item_eoi
    module_expr module_expr_eoi
    module_type module_type_eoi
    ref_t ref_t_eoi
    top_binding top_binding_eoi
    top_bindings top_bindings_eoi
    ;

    mid: [ [ uid = UIDENT -> ID.of_string uid ] ] ;
    base_type: [ [
        "null" -> JNull
      | "string" -> JString
      | "boolean" -> JBool
      | "number" -> JNumber
      | "array" -> JArray
      | "object" -> JObject
      ] ]
    ;

    size_constraint: [ [
        lexcl = [ "[" -> False | "(" -> True ] ; n = INT ; "," ;
        m = [ m = INT -> Some (int_of_string m) | "max" -> None ] ;
        rexcl= [ "]" -> False | ")" -> True ] ->
        let n = int_of_string n in
        (Bound.{ it=n ; exclusive = lexcl }, 
         Bound.{ it=m ; exclusive = rexcl })
      ] ]
    ;

    number: [ [ n = FLOAT -> n | n = INT -> n ] ] ;
    range_constraint: [ [
        lexcl = [ "[" -> False | "(" -> True ] ;
        n = [ n = number -> Some (float_of_string n) | "min" -> None ] ; "," ;
        m = [ m = number -> Some (float_of_string m) | "max" -> None ] ;
        rexcl= [ "]" -> False | ")" -> True ] ->
        (Bound.{ it=n ; exclusive = lexcl }, 
         Bound.{ it=m ; exclusive = rexcl })
      ] ]
    ;

    atomic_utype: [ [
        "required" ; id = STRING ; "," ; l = LIST0 STRING SEP "," -> [FieldRequired loc [id::l]]
      | "required" ; id = STRING -> [FieldRequired loc [id]]
      | "required" ; id = STRING ; ":" ; t = utype -> [Field loc id t;FieldRequired loc [id]]
      | id = STRING ; ":" ; t = utype -> [Field loc id t]
      | re = REGEXP -> [StringRE loc re]
      | "of" ; t = utype -> [ArrayOf loc t]
      | l = LIST1 utype SEP "*" -> [ArrayTuple loc l]
      | "unique" -> [ArrayUnique loc]
      | n=INT ; ":" ; t=utype -> [ArrayIndex loc (int_of_string n) t]
      | "size" ; s=size_constraint -> [Size loc s]
      | "bounds" ; s=range_constraint -> [NumberBound loc s]
      | "multipleOf" ; n = FLOAT -> [MultipleOf loc (float_of_string n)]
      | "enum" ; l = LIST1 json SEP "," -> [Enum loc (l |> List.map LJ.to_json |> List.map canon_json)]
      | "default" ; j=json -> [Default loc (LJ.to_json j)]
      | "format" ; s=STRING -> [Format loc s]
      | "propertyNames" ; t = utype -> [PropertyNames loc t]
      | "contentMediaType" ; s=STRING -> [ContentMediaType loc s]
      | "contentEncoding" ; s=STRING -> [ContentEncoding loc s]
      ] ]
    ;

    utype: [
      "||" RIGHTA [
        t1 = utype ; "||" ; t2 = utype -> Or loc t1 t2
      ]
    | "xor" RIGHTA [
        t1 = utype ; "xor" ; t2 = utype -> Xor loc t1 t2
      ]
    | "&&" RIGHTA [
        t1 = utype ; "&&" ; t2 = utype -> And loc t1 t2
      ]
    | "=>" RIGHTA [
        t1 = utype ; "=>" ; t2 = utype -> Impl loc t1 t2
      ]
    | "not" [
        "not" ; t = utype -> Not loc t
      ]
    | "seal" [
        "seal" ; t = utype LEVEL "simple" ->
        Seal loc t [] (UtFalse loc)
      | "seal" ; t = utype LEVEL "simple" ; "with" ; (l,orelse) = seal_extras ->
        Seal loc t (List.stable_sort Stdlib.compare l) orelse
      ]
    | "simple" [
        b = base_type -> Simple loc b
      | "true" -> UtTrue loc
      | "false" -> UtFalse loc
      | (mpopt, tid) = tid_path -> Ref loc (mpopt, tid)
      | "[" ; h = atomic_utype ; ";" ; l = atomic_utype_semi_list ; "]" -> Atomic loc (h@l)
      | "[" ; h = atomic_utype ; "]" -> Atomic loc h
      | "(" ; t = utype ; ")" -> t
      ]
    ]
    ;
    seal_extras: [ [
        r = REGEXP ; ":" ; t = utype LEVEL "simple"  -> ([(r,t)],UtFalse loc)
      | t = utype LEVEL "simple" -> ([],t)
      | r = REGEXP ; ":" ; t = utype LEVEL "simple" ; "," ; (l,orelse) = seal_extras -> ([(r,t)::l],orelse)

      ] ]
    ;

    atomic_utype_semi_list:
      [ [
        v = atomic_utype -> v
      | v = atomic_utype ; ";" -> v
      | v = atomic_utype ; ";" ; vl = atomic_utype_semi_list -> v@vl
      | -> []
      ] ]
    ;

    module_type: [ [
        "sig" ; l = signature ; "end" -> MtSig loc l
      | "functor" ; l = LIST1 module_param ; "->" ; m=module_type ->
        List.fold_right (fun (s,mty) rhs -> MtFunctorType loc (s,mty) rhs) l m
      | p = module_path ->
        match p with [
          TOP _ -> Fmt.(failwithf "module_type: cannot be a TOP module-id")
        | REL h -> MtPath loc (None, h)
        | DEREF p last -> MtPath loc (Some p, last)
        ]
      ] ]
    ;

    tid_path: [ [
        l = LIST0 [ s = mid ; "." -> s ] ; id = LIDENT ->
        match l with [
          [] -> (None, ID.of_string id)
        | l ->
          let mp = make_module_path False l in
          (Some mp, ID.of_string id)
        ]
      | "." ; l = LIST1 [ s = mid ; "." -> s ] ; id = LIDENT ->
        (Some (make_module_path True l), ID.of_string id)
      ] ]
    ;

    module_path: [ [
        p = LIST1 mid SEP "." -> make_module_path False p
      | "." ; p = LIST1 mid SEP "." -> make_module_path True p
      ] ]
    ;

    module_expr: [
      "top" LEFTA [
        m1 = module_expr ; "(" ; m2 = module_expr ; ")" -> MeFunctorApp loc m1 m2
      | m1 = module_expr ; ":" ; mt = module_type -> MeCast loc m1 mt
      ]
    | "simple" [
        "struct" ; l = structure ; "end" -> MeStruct loc l
      | "functor" ; l = LIST1 module_param ; "->" ; m=module_expr ->
        List.fold_right (fun (s,mty) rhs -> MeFunctor loc (s,mty) rhs) l m
      | p = module_path -> MePath loc p
      | "(" ; me = module_expr ; ")" -> me
      ]
    ]
    ;
    module_param: [ [
        "(" ; id = mid ; ":" ; mty = module_type ; ")" -> (id, mty)
      ] ]
    ;

    annotation: [ [
        "[" ; "sealed" ; "," ; l = LIST1 base_type SEP "," ; "]" -> AN.mk loc l True
      | "[" ; l = LIST1 base_type SEP "," ; "]" -> AN.mk loc l False
      ] ]
    ;
    annotation_opt: [ [ x = OPT annotation -> x ] ]
    ;
    struct_item: [ [
        "module" ; uid=mid ; "=" ; e = module_expr ; ";" -> StModuleBinding loc uid e
      | "module" ; "type" ; uid=mid ; "=" ; e = module_type ; ";" -> StModuleType loc uid e
      | "local" ; l1 = structure ; "in" ; l2 = structure ; "end" ; ";" -> StLocal loc l1 l2
      | "type" ; rflag = [ "rec" -> True | "nonrec" -> False | -> False ] ;
        l = LIST1 [ anno_opt = annotation_opt ; id = LIDENT ; "=" ; t = utype -> (ID.of_string id, anno_opt, t) ] SEP "and" ;
        ";" -> StTypes loc rflag l
      | "import" ; s=STRING ; "as"; uid=mid ; ";" -> StImport loc s uid
      | "open" ; p = module_path ; ";" -> StOpen loc p None
      | "open" ; p = module_path ; ":" ; t = module_type ; ";" -> StOpen loc p (Some t)
      | "include" ; p = module_path ; ";" -> StInclude loc p None
      | "include" ; p = module_path ; ":" ; t = module_type ; ";" -> StInclude loc p (Some t)
      ] ]
    ;
    sig_item: [ [
        "type" ; l = LIST1 [ anno = annotation ; id = LIDENT -> (id, anno) ] SEP "," ; ";" ->
        l |> List.map (fun (s,anno) -> SiType loc (ID.of_string s) anno)
      | "module" ; uid = mid ; ":" ; mty=module_type ; ";" -> [SiModuleBinding loc uid mty]
      | "module" ; "type" ; uid = mid ; "=" ; mty=module_type ; ";" -> [SiModuleType loc uid mty]
      | "include" ; p = module_path ; ";" -> [SiInclude loc p]
      ] ]
    ;

    structure: [ [
        l = LIST0 struct_item -> l
      ] ]
    ;

    signature: [ [
        l = LIST0 sig_item -> List.concat l
      ] ]
    ;


  json:
    [ [ s = scalar -> s

      | "[" ; l = LIST0 json SEP "," ; "]" -> List loc l
      | "{" ; l = LIST0 [ s = STRING ; ":" ; v=json -> (s,v) ] SEP "," ; "}" -> Assoc loc l
    ] ]
  ;

  scalar:
    [ [ n = INT -> Int loc (int_of_string n)
      | n = FLOAT -> Float loc (float_of_string n)
      | s = STRING -> String loc s
      | "null" -> Null loc
      | "true" -> Bool loc True
      | "false" -> Bool loc False
    ] ]
  ;

  ref_: [ [
      id = LIDENT -> (None, ID.of_string id)
    | "." ; l = LIST1 [ s = mid ; "." -> s ] ; id = LIDENT ->
      (Some (make_module_path True l), ID.of_string id)
    ] ]
  ;
  top_binding: [ [
      a = annotation ; r = ref_ ; "=" ; ut = utype ; ";" -> (loc, r, a, ut)
    ] ]
  ;
  top_bindings: [ [
      l = LIST0 top_binding -> l
    ] ]
  ;


  json_eoi : [ [ e = json ; EOI -> e ] ] ;
  annotation_eoi : [ [ e = annotation ; EOI -> e ] ] ;
  utype_eoi : [ [ e = utype ; EOI -> e ] ] ;
  structure_eoi : [ [ e = structure ; EOI -> e ] ] ;
  struct_item_eoi : [ [ e = struct_item ; EOI -> e ] ] ;
  signature_eoi : [ [ e = signature ; EOI -> e ] ] ;
  sig_item_eoi : [ [ e = sig_item ; EOI -> e ] ] ;
  module_expr_eoi : [ [ e = module_expr ; EOI -> e ] ] ;
  module_type_eoi : [ [ e = module_type ; EOI -> e ] ] ;
  ref_t_eoi : [ [ e = ref_t ; EOI -> e ] ] ;
  top_binding_eoi : [ [ e = top_binding ; EOI -> e ] ] ;
  top_bindings_eoi : [ [ e = top_bindings ; EOI -> e ] ] ;
END;

value parse_json = Grammar.Entry.parse json ;
value parse_json_eoi = Grammar.Entry.parse json_eoi ;

value parse_annotation = Grammar.Entry.parse annotation ;
value parse_annotation_eoi = Grammar.Entry.parse annotation_eoi ;

value parse_utype = Grammar.Entry.parse utype ;
value parse_utype_eoi = Grammar.Entry.parse utype_eoi ;

value parse_structure = Grammar.Entry.parse structure ;
value parse_structure_eoi = Grammar.Entry.parse structure_eoi ;

value parse_struct_item = Grammar.Entry.parse struct_item ;
value parse_struct_item_eoi = Grammar.Entry.parse struct_item_eoi ;

value parse_signature = Grammar.Entry.parse signature ;
value parse_signature_eoi = Grammar.Entry.parse signature_eoi ;

value parse_sig_item = Grammar.Entry.parse sig_item ;
value parse_sig_item_eoi = Grammar.Entry.parse sig_item_eoi ;

value parse_module_expr = Grammar.Entry.parse module_expr ;
value parse_module_expr_eoi = Grammar.Entry.parse module_expr_eoi ;

value parse_module_type = Grammar.Entry.parse module_type ;
value parse_module_type_eoi = Grammar.Entry.parse module_type_eoi ;

value parse_ref_t = Grammar.Entry.parse ref_t ;
value parse_ref_t_eoi = Grammar.Entry.parse ref_t_eoi ;

value parse_top_binding = Grammar.Entry.parse top_binding ;
value parse_top_binding_eoi = Grammar.Entry.parse top_binding_eoi ;

value parse_top_bindings = Grammar.Entry.parse top_bindings ;
value parse_top_bindings_eoi = Grammar.Entry.parse top_bindings_eoi ;

value parse_string pf s = do {
  input_file.val := s ;
  pf (Stream.of_string s)
}
;

value parse_channel pf ic = do {
  input_file.val := "<channel-input>" ;
  pf (Stream.of_channel ic)
}
;

value parse_file pf fname = do {
  input_file.val := fname ;
  let ic = open_in fname in
  let rv = pf (Stream.of_channel ic) in 
  do { close_in ic ; rv }
}
;
