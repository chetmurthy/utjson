open OUnit2
open OUnitTest
open Pa_ppx_testutils
open Pa_ppx_utils.Std

open Uttestutil
open Utypes
open Ututil
open Utparse0
open Utprint
open Utio
open Utio.Debug
open Utconv
open Utextract
open Uttypecheck

let success (expect, f) =
  assert_equal ~msg:f ~printer:structure_printer ~cmp:structure_cmp
    (structure_of_string_exn expect)
    (convert_file ~with_predefined:true (CC.mk()) f)

let successf (expectf, f) =
  assert_equal ~msg:f ~printer:structure_printer ~cmp:structure_cmp
    (load_file ~with_predefined:true  expectf)
    (convert_file ~with_predefined:true (CC.mk())  f)

let convert1 f =
  let cc = CC.mk ~filepath:["utj-generated"] () in
  let f = select_schema_file f in
  convert_file ~with_predefined:true cc f

let convert_check1 f =
  f >:: (fun ctxt ->
      ignore(convert1 f)
    )

let convert_check = "convert_check" >::: (List.map convert_check1 all_schemastore_files)

let typecheck1 f =
  f >:: (fun ctxt ->
      let stl = convert1 f in
      ignore (S4Typecheck.exec stl)
    )

let typecheck = "typecheck" >::: (List.map typecheck1 all_schemastore_files)

let extract1 f =
  f >:: (fun ctxt ->
      let stl = convert1 f in
      ignore (stl |> full_extract |> FinalExtract.exec)
    )

let extract = "extract" >::: (List.map extract1 all_schemastore_files)

let tests = "all" >::: [
    convert_check
  ; typecheck
  ; extract
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
