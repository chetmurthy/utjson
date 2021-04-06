open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]

] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value input_file = Plexing.input_file ;

value lexer = Plexer.gmake () ;
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
