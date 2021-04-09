open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Utypes
open Utparse0
open Utprint
open Utconv
open Uttypecheck

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp (Str.quote pattern)) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern ?msg pattern f =
  Testutil.assert_raises_exn_pred ?msg
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

let of_string_exn s = s |> parse_string parse_utype_eoi
let to_string t = print_utype Pprintf.empty_pc t

let struct_item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let struct_item_to_string t = print_struct_item Pprintf.empty_pc t

let sig_item_of_string_exn s = s |> parse_string parse_sig_item_eoi
let sig_item_to_string t = print_sig_item Pprintf.empty_pc t

let structure_of_string_exn s = s |> parse_string parse_structure_eoi
let structure_to_string t = print_structure Pprintf.empty_pc t

let signature_of_string_exn s = s |> parse_string parse_signature_eoi
let signature_to_string t = print_signature Pprintf.empty_pc t

let module_expr_of_string_exn s = s |> parse_string parse_module_expr_eoi
let module_expr_to_string t = print_module_expr Pprintf.empty_pc t

let module_type_of_string_exn s = s |> parse_string parse_module_type_eoi
let module_type_to_string t = print_module_type Pprintf.empty_pc t

let printer = show_utype_t
let cmp = equal_utype_t

let struct_item_printer x = "<<"^(show_struct_item_t x)^">>"
let struct_item_cmp = equal_struct_item_t


let module_expr_printer x = "<<"^(show_module_expr_t x)^">>"
let module_expr_cmp = equal_module_expr_t

let module_type_printer x = "<<"^(show_module_type_t x)^">>"
let module_type_cmp = equal_module_type_t

let sig_item_printer x = "<<"^(show_sig_item_t x)^">>"
let sig_item_cmp = equal_sig_item_t

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let typing = "typing" >::: [
    "1" >:: (fun ctxt -> 
        let st = structure_of_string_exn {|
module type Ext1 = sig extension ; end ;
module ExtensibleTree = functor( M : Ext1 ) -> struct
  type t = object && [ "data" : object ; "children" : array && [ of t ] ] && M.extension ;
end ;

module StrictTree = ExtensibleTree( struct type extension = [ sealed ] ; end ) ;
|} in
        ignore(tc_structure [] st)
      )
  ]


let tests = "all" >::: [
    simple
  ; typing
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
