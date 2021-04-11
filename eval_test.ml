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
open Uteval

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let expand_import = "expand-import" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local module Predefined = struct
  type nonrec integer = number && [ multipleOf 1.000000; ];
    type nonrec scalar = boolean || number || string;
    type nonrec json = null || scalar || array || object;
  end : sig integer; scalar; json; end; in  end;
|} |> structure_of_string_exn )
        ({|
local import "lib/predefined.utj" as Predefined; in
end ;
|} |> structure_of_string_exn |> S2Typecheck.exec)
      )
  ]


let tests = "all" >::: [
    simple
  ; expand_import
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
