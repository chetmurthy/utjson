open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;
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

value (utype : Grammar.Entry.e utype_t) = Grammar.Entry.create g "utype";
value (utype_eoi : Grammar.Entry.e utype_t) = Grammar.Entry.create g "utype_eoi";

value (structure : Grammar.Entry.e structure) = Grammar.Entry.create g "structure";
value (structure_eoi : Grammar.Entry.e structure) = Grammar.Entry.create g "structure_eoi";

value (structure_item : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "structure_item";
value (structure_item_eoi : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "structure_item_eoi";

value (signature : Grammar.Entry.e signature) = Grammar.Entry.create g "signature";
value (signature_eoi : Grammar.Entry.e signature) = Grammar.Entry.create g "signature_eoi";

value (signature_item : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "signature_item";
value (signature_item_eoi : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "signature_item_eoi";

EXTEND
  GLOBAL:
    utype utype_eoi
    structure structure_eoi
    structure_item structure_item_eoi
    signature signature_eoi
    signature_item signature_item_eoi
    ;

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
        fname = STRING ; ":" ; t = utype -> Field fname t
      | re = REGEXP ; ":" ; t = utype -> FieldRE (String.sub re 1 (String.length re - 2)) t
      | re = REGEXP -> StringRE (String.sub re 1  (String.length re - 2))
      | "required" ; l = LIST1 STRING SEP "," -> FieldRequired l
      | "of" ; t = utype -> ArrayOf t
      | l = LIST1 utype SEP "*" -> ArrayTuple l
      | "unique" -> ArrayUnique
      | n=INT ; ":" ; t=utype -> ArrayIndex (int_of_string n) t
      | "size" ; s=size_constraint -> Size s
      | "bounds" ; s=range_constraint -> NumberBound s
      | "sealed" -> Sealed True
      | "unsealed" -> Sealed False
      | "orelse" ; t=utype -> OrElse t
      | "multipleOf" ; n = FLOAT -> MultipleOf (float_of_string n)
      | "enum" ; l = LIST1 json SEP "," -> Enum l
      | "default" ; j=json -> Default j
      | "format" ; s=STRING -> Format s
      | "propertyNames" ; t = utype -> PropertyNames t
      | "contentMediaType" ; s=STRING -> ContentMediaType s
      | "contentEncoding" ; s=STRING -> ContentEncoding s
      ] ]
    ;

    utype: [
      "||" RIGHTA [
        t1 = utype ; "||" ; t2 = utype -> Or t1 t2
      ]
    | "xor" RIGHTA [
        t1 = utype ; "xor" ; t2 = utype -> Xor t1 t2
      ]
    | "&&" RIGHTA [
        t1 = utype ; "&&" ; t2 = utype -> And t1 t2
      ]
    | "=>" RIGHTA [
        t1 = utype ; "=>" ; t2 = utype -> Impl t1 t2
      ]
    | "not" [
        "not" ; t = utype -> Not t
      ]
    | "simple" [
        b = base_type -> Simple b
      | l = LIST0 [ s = UIDENT ; "." -> s ] ; id = LIDENT -> Ref l id
      | "[" ; h = atomic_utype ; ";" ; l = atomic_utype_semi_list ; "]" -> Atomic [h::l]
      | "[" ; h = atomic_utype ; "]" -> Atomic [h]
      | "(" ; t = utype ; ")" -> t
      ]
    ]
    ;


    atomic_utype_semi_list:
      [ [
        v = atomic_utype -> [v]
      | v = atomic_utype ; ";" -> [v]
      | v = atomic_utype ; ";" ; vl = atomic_utype_semi_list -> [v::vl]
      | -> []
      ] ]
    ;

    module_type: [ [
        "sig" ; l = signature ; "end" -> Sig l
      | "functor" ; l = LIST1 module_param ; "->" ; m=module_type ->
        List.fold_right (fun (s,mty) rhs -> FunctorType (s,mty) rhs) l m
      ] ]
    ;

    module_path: [ [
        p = LIST1 UIDENT SEP "." -> p
      ] ]
    ;

    module_expr: [ [
        "struct" ; l = structure ; "end" -> Struct l
      | m1 = module_expr ; "(" ; m2 = module_expr ; ")" -> FunctorApp m1 m2
      | "functor" ; l = LIST1 module_param ; "->" ; m=module_expr ->
        List.fold_right (fun (s,mty) rhs -> Functor (s,mty) rhs) l m
      | p = module_path -> ModulePath p
      ] ]
    ;
    module_param: [ [
        "(" ; id = UDENT ; mty = module_type ; ")" -> (id, mty)
      ] ]
    ;

    structure_item: [ [
        "module" ; uid=UIDENT ; "=" ; e = module_expr ; ";" -> StModuleBinding uid e
      | "module" ; "type" ; uid=UIDENT ; "=" ; e = module_type ; ";" -> StModuleType uid e
      | "local" ; l1 = structure ; "in" ; l2 = structure ; "end" ; ";" -> StLocal l1 l2
      | "type" ; rflag = [ "rec" -> True | "nonrec" -> False | -> False ] ;
        l = LIST1 [ id = LIDENT ; "=" ; t = utype -> (id, t) ] SEP "and" ;
        ";" -> StTypes rflag l
      | "import" ; s=STRING ; "as"; uid=UIDENT ; ";" -> StImport s uid
      | "open" ; p = module_path ; ";" -> StOpen p
      | "include" ; p = module_path ; ";" -> StInclude p
      ] ]
    ;

    structure: [ [
        l = LIST0 structure_item -> l
      ] ]
    ;


  json:
    [ [ s = scalar -> s

      | "[" ; l = LIST0 json SEP "," ; "]" -> `List l
      | "{" ; l = LIST0 [ s = STRING ; ":" ; v=json -> (s,v) ] SEP "," ; "}" -> `Assoc l
    ] ]
  ;

  scalar:
    [ [ n = INT -> `Int (int_of_string n)
      | n = FLOAT -> `Float (float_of_string n)
      | s = STRING -> `String s
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  utype_eoi : [ [ e = utype ; EOI -> e ] ] ;
  structure_eoi : [ [ e = structure ; EOI -> e ] ] ;
  structure_item_eoi : [ [ e = structure_item ; EOI -> e ] ] ;
END;

value parse_utype = Grammar.Entry.parse utype ;
value parse_utype_eoi = Grammar.Entry.parse utype_eoi ;

value parse_structure = Grammar.Entry.parse structure ;
value parse_structure_eoi = Grammar.Entry.parse structure_eoi ;

value parse_structure_item = Grammar.Entry.parse structure_item ;
value parse_structure_item_eoi = Grammar.Entry.parse structure_item_eoi ;

value parse_signature = Grammar.Entry.parse signature ;
value parse_signature_eoi = Grammar.Entry.parse signature_eoi ;

value parse_signature_item = Grammar.Entry.parse signature_item ;
value parse_signature_item_eoi = Grammar.Entry.parse signature_item_eoi ;

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
