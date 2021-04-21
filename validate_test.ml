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
open Utextract
open Utsimplify
open Utvalidate

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ; "product" >:: (fun ctxt ->
        let tdl = 
          let filepath = ["schema-golden/schema-overrides"] in
          let f = "schema-overrides/product-schema.json" in
          FinalExtract.exec (full_extract (convert_file ~with_predefined:true (CC.mk ~filepath ()) f)) in
        assert_bool "should be true" (validate tdl (Yojson.Basic.from_file "schema-overrides/product.json") (None, ID.of_string "t"))
      )
  ]




let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
