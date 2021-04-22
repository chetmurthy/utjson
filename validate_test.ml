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

let load_and_convert cc f =
  if Fpath.(f |> v |> has_ext "utj") then
    load_file f
  else convert_file ~with_predefined:true cc f

let validate_file ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(tid=(None, ID.of_string "t")) () =
  let tdl = 
    FinalExtract.exec (full_extract (load_and_convert (CC.mk ~filepath ()) schema)) in
  validate tdl (Yojson.Basic.from_file instance) tid

let validate_json ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(tid=(None, ID.of_string "t")) () =
  let tdl = 
    FinalExtract.exec (full_extract (load_and_convert (CC.mk ~filepath ()) schema)) in
  validate tdl (Yojson.Basic.from_string instance) tid

let success_file (schema, instance) =
  (schema^" || "^instance) >:: (fun ctxt ->
      assert_bool "should be true" (validate_file
                                      ~schema
                                      ~instance
                                      ())
    )

let success_json (schema, instance) =
  (schema^" || "^instance) >:: (fun ctxt ->
      assert_bool "should be true" (validate_json
                                      ~schema
                                      ~instance
                                      ())
    )

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ; success_file ("schema-overrides/product-schema.json","schema-overrides/product.json")
  ; success_file ("schema-overrides/ansible-inventory-FIXED.utj","schemastore/src/test/ansible-inventory/inventory.json")
  ; success_file ("schema-overrides/ansible-inventory-FIXED.utj","schemastore/src/test/ansible-inventory/inventory-2.json")
  ; success_file ("schema-overrides/ansible-playbook.json","schemastore/src/test/ansible-playbook/playbook-1.json")
  ]




let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
