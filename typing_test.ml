open OUnit2
open OUnitTest
open Pa_ppx_testutils
open Uttestutil

open Utypes
open Utparse0
open Utprint
open Utio
open Utio.Debug
open Utconv
open Uttypecheck

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let successf (expect_sil_s, expect_stl_s, stl_s) =
  let stl = structure_of_string_exn stl_s in
  let expect_stl = Option.map structure_of_string_exn expect_stl_s in
  let expect_sil = Option.map signature_of_string_exn expect_sil_s in
  let (_, (res_stl, res_sil)) = tc_structure Env.mt stl in
  (match expect_sil with None -> () | Some expect_sil ->
      assert_equal ~printer:Normal.signature_printer ~cmp:signature_cmp expect_sil res_sil) ;
  (match expect_stl with None -> () | Some expect_stl ->
      assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp expect_stl res_stl)

let success_test (expect_sil_s, expect_stl_s, stl_s) =
  stl_s >:: (fun ctxt ->
      successf(expect_sil_s, expect_stl_s, stl_s)
    )

let typing = "typing" >::: [
    "1" >:: (fun ctxt -> 
        let st = structure_of_string_exn {|
module type Ext1 = sig extension ; end ;
module ExtensibleTree = functor( M : Ext1 ) -> struct
  type rec t = object && [ "data" : object ; "children" : array && [ of t ] ] && M.extension ;
end ;

module StrictTree = ExtensibleTree( struct type extension = [ sealed ] ; end ) ;
|} in
        ignore(tc_structure Env.mt st)
      )
  ; "success" >::: (List.map success_test [
        (Some {|
module type Ext1 = sig extension; end;
module ExtensibleTree : functor (M:sig extension; end) -> sig t; end;
module StrictTree : sig t; end;
|},
Some {|
module type Ext1 = sig extension; end;
  module ExtensibleTree = functor (M:sig extension; end) -> struct
    type rec t = object && [
        "data": object;
        "children": array && [ of t; ];
] && M.extension;
    end;
  module StrictTree = ExtensibleTree(struct type nonrec extension = [ sealed; ]; end) : sig t; end;
|},
{|
module type Ext1 = sig extension ; end ;
module ExtensibleTree = functor( M : Ext1 ) -> struct
  type rec t = object && [ "data" : object ; "children" : array && [ of t ] ] && M.extension ;
end ;

module StrictTree = ExtensibleTree( struct type extension = [ sealed ] ; end ) ;
|})
        ;         (Some {|
module type T1 = sig extension; end;
  module type T2 = functor (M:sig extension; end) -> sig extension; end;
|},
                   Some {|
module type T1 = sig extension; end;
  module type T2 = functor (M:sig extension; end) -> sig extension; end;
|},
{|
module type T1 = sig extension ; end ;
module type T2 = functor (M:T1) -> T1 ;
|})
      ])
  ]


let tests = "all" >::: [
    simple
  ; typing
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
