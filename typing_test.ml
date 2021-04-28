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

let to_ag stl =
  let dt_to_ag = Utmigrate2.ToAG.make_dt () in
  dt_to_ag.migrate_structure dt_to_ag stl

let from_ag stl =
  let dt_from_ag = Utmigrate2.FromAG.make_dt () in
  dt_from_ag.migrate_structure dt_from_ag stl

let evaluate stl =
  Utypes2.REC.AG.Ordered.evaluate stl

let successf (expect_sil_s, expect_stl_s, stl_s) =
  let stl = structure_of_string_exn stl_s in
  let a_stl = to_ag stl in
  let stl' = evaluate a_stl in
  let stl'' = from_ag a_stl in
  assert_equal ~msg:"evaluate failed"
    ~printer:Normal.structure_printer ~cmp:Reloc.(wrap_cmp structure_cmp structure) stl stl' ;
  assert_equal ~msg:"roundtrip failed"
    ~printer:Normal.structure_printer ~cmp:Reloc.(wrap_cmp structure_cmp structure) stl stl'' ;
  let expect_stl = Option.map structure_of_string_exn expect_stl_s in
  let expect_sil = Option.map signature_of_string_exn expect_sil_s in
  let (_, (res_stl, res_sil)) = tc_structure TEnv.mt stl in
  (match expect_sil with None -> () | Some expect_sil ->
      assert_equal ~printer:Normal.signature_printer ~cmp:Reloc.(wrap_cmp signature_cmp signature) expect_sil res_sil) ;
  (match expect_stl with None -> () | Some expect_stl ->
      assert_equal ~printer:Normal.structure_printer ~cmp:Reloc.(wrap_cmp structure_cmp structure) expect_stl res_stl)

let success_test (expect_sil_s, expect_stl_s, stl_s) =
  stl_s >:: (fun ctxt ->
      successf(expect_sil_s, expect_stl_s, stl_s)
    )

let typing = "typing" >::: [
    "1" >:: (fun ctxt -> 
        let st = structure_of_string_exn {|
module type Ext1 = sig type extension ; end ;
module ExtensibleTree = functor( M : Ext1 ) -> struct
  type rec t = object && [ "data" : object ; "children" : array && [ of t ] ] && M.extension ;
end ;

module StrictTree = ExtensibleTree( struct type extension = [ sealed ] ; end ) ;
|} in
        ignore(tc_structure TEnv.mt st)
      )
  ; "success" >::: (List.map success_test [
        (Some {|
module type Ext1 = sig type extension; end;
module ExtensibleTree : functor (M:sig type extension; end) -> sig type t; end;
module StrictTree : sig type t; end;
|},
Some {|
module type Ext1 = sig type extension; end;
  module ExtensibleTree = functor (M:sig type extension; end) -> struct
    type rec t = object && [
        "data": object;
        "children": array && [ of t; ];
] && M.extension;
    end;
  module StrictTree = ExtensibleTree(struct type nonrec extension = [ sealed; ]; end) : sig type t; end;
|},
{|
module type Ext1 = sig type extension ; end ;
module ExtensibleTree = functor( M : Ext1 ) -> struct
  type rec t = object && [ "data" : object ; "children" : array && [ of t ] ] && M.extension ;
end ;

module StrictTree = ExtensibleTree( struct type extension = [ sealed ] ; end ) ;
|})
        ;         (Some {|
module type T1 = sig type extension; end;
  module type T2 = functor (M:sig type extension; end) -> sig type extension; end;
|},
                   Some {|
module type T1 = sig type extension; end;
  module type T2 = functor (M:sig type extension; end) -> sig type extension; end;
|},
{|
module type T1 = sig type extension ; end ;
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
