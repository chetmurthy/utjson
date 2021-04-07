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
  | Regexp n -> ("REGEXP",n)
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

value (utype_structure : Grammar.Entry.e structure) = Grammar.Entry.create g "utype_structure";
value (utype_structure_eoi : Grammar.Entry.e structure) = Grammar.Entry.create g "utype_structure_eoi";

value (utype_structure_item : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "utype_structure_item";
value (utype_structure_item_eoi : Grammar.Entry.e struct_item_t) = Grammar.Entry.create g "utype_structure_item_eoi";

EXTEND
  GLOBAL:
    utype utype_eoi
    utype_structure utype_structure_eoi
    utype_structure_item utype_structure_item_eoi
    ;

    base_type: [ [
        "null" -> JNull
      | "string" -> JString
      | "bool" -> JBool
      | "number" -> JNumber
      | "array" -> JArray
      | "object" -> JObject
      ] ]
    ;

    size_constraint: [ [
        lincl = [ "[" -> True | "(" -> False ] ; n = INT ; "," ;
        m = [ m = INT -> Some (int_of_string m) | "max" -> None ] ;
        rincl= [ "]" -> True | ")" -> False ] ->
        let n = int_of_string n in
        (Bound.{ it=n ; inclusive = lincl }, 
         Bound.{ it=m ; inclusive = rincl })
      ] ]
    ;

    range_constraint: [ [
        lincl = [ "[" -> True | "(" -> False ] ;
        n = [ n = FLOAT -> Some (float_of_string n) | "min" -> None ] ; "," ;
        m = [ m = FLOAT -> Some (float_of_string m) | "max" -> None ] ;
        rincl= [ "]" -> True | ")" -> False ] ->
        (Bound.{ it=n ; inclusive = lincl }, 
         Bound.{ it=m ; inclusive = rincl })
      ] ]
    ;

    atomic_utype: [ [
        fname = STRING ; ":" ; t = utype  -> Field fname t
      | re = REGEXP ; ":" ; t = utype -> FieldRE re t
      | re = REGEXP -> StringRE re
      | "required" ; l = LIST1 STRING SEP "," ; ";" -> FieldRequired l
      | "of" ; t = utype ; ";" -> ArrayOf t
      | l = LIST1 utype SEP "*" ; ";" -> ArrayTuple l
      | "unique" ; ";" -> ArrayUnique
      | n=INT ; ":" ; t=utype ; ";" -> ArrayIndex (int_of_string n) t
      | "size" ; s=size_constraint ; ";" -> Size s
      | "bounds" ; s=range_constraint ; ";" -> NumberBound s
      | "sealed" ; ";" -> Sealed True
      | "unsealed" ; ";" -> Sealed False
      | "orelse" ; t=utype ; ";" -> OrElse t
      ] ]
    ;

    utype: [
      "||" RIGHTA [
        t1 = utype ; "||" ; t2 = utype -> Or t1 t2
      ]
    | "&&" RIGHTA [
        t1 = utype ; "&&" ; t2 = utype -> And t1 t2
      ]
    | "not" [
        "not" ; t = utype -> Not t
      ]
    | "simple" [
        b = base_type -> Simple b
      | l = LIST0 [ s = UIDENT ; "." -> s ] ; id = LIDENT -> Ref l id
      | "[" ; l = LIST1 atomic_utype ; "]" -> Atomic l
      ]
    ]
    ;

    utype_structure_item: [ [
        "module" ; uid=UIDENT ; "=" ; "struct" ;
        l = utype_structure ; ";" -> Module uid l
      | "local" ; l1 = utype_structure ; "in" ; l2 = utype_structure ; ";" -> Local l1 l2
      | "type" ; id = LIDENT ; "=" ; t = utype ; ";" -> Decl id t
      ] ]
    ;

    utype_structure: [ [
        l = LIST0 utype_structure_item -> l
      ] ]
    ;


  utype_eoi : [ [ e = utype ; EOI -> e ] ] ;
  utype_structure_eoi : [ [ e = utype_structure ; EOI -> e ] ] ;
  utype_structure_item_eoi : [ [ e = utype_structure_item ; EOI -> e ] ] ;
END;

value parse_utype = Grammar.Entry.parse utype ;
value parse_utype_eoi = Grammar.Entry.parse utype_eoi ;

value parse_utype_structure = Grammar.Entry.parse utype_structure ;
value parse_utype_structure_eoi = Grammar.Entry.parse utype_structure_eoi ;

value parse_utype_structure_item = Grammar.Entry.parse utype_structure_item ;
value parse_utype_structure_item_eoi = Grammar.Entry.parse utype_structure_item_eoi ;

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
