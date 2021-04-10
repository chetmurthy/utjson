open OUnit2
open OUnitTest
open Pa_ppx_testutils
open Uttestutil

open Utypes
open Utparse0
open Utprint
open Utio
open Utconv
open Uttypecheck

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let expand_import = "expand-import" >::: [
    "simple" >:: (fun ctxt -> 
        ()
      )
  ]


let tests = "all" >::: [
    simple
  ; expand_import
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
