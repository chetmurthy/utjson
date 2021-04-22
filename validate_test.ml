open OUnit2
open OUnitTest
open Pa_ppx_utils.Std
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

let schema_test_pairs schema =
  let schemafile = select_schema_file schema in
  let testroot = Fpath.("schemastore/src/test" |> v) in
  let dirname = Fpath.(schema |> v |> rem_ext) in
  let testfiles =
    let testdir = Fpath.(append testroot dirname) in
    if testdir |> Bos.OS.Dir.exists |> Rresult.R.get_ok then
      testdir
      |> Bos.OS.Dir.contents
      |> Rresult.R.get_ok
      |> List.map Fpath.to_string
    else []
  in
  testfiles |> List.map (fun t -> (schemafile, t))

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let exceptions = "exceptions" >::: [
    success_file ("schema-overrides/product-schema.json","schema-overrides/product.json")
  ; success_file ("schema-overrides/ansible-inventory-FIXED.utj","schemastore/src/test/ansible-inventory/inventory.json")
  ; success_file ("schema-overrides/ansible-inventory-FIXED.utj","schemastore/src/test/ansible-inventory/inventory-2.json")
  ; success_file ("schema-overrides/azure-iot-edge-deployment-template-2.0-FIXED.utj",
                  "schemastore/src/test/azure-iot-edge-deployment-template-2.0/deployment.template.json")
  ; success_file ("schema-overrides/azure-iot-edge-deployment-template-1.0-FIXED.utj",
                  "schemastore/src/test/azure-iot-edge-deployment-template-1.0/deployment.template.json")
]

let testfiles = subtract all_schemastore_files [
    "ansible-inventory.json"
  ; "azure-iot-edge-deployment-template-2.0.json"
  ; "azure-iot-edge-deployment-template-1.0.json"
  ]
let testfiles = (firstn 60 testfiles)
let schemastore = "schemastore" >::: (
   testfiles |> List.concat_map schema_test_pairs |> List.map success_file
  )

let tests = "all" >::: [
    simple
  ; exceptions
  ; schemastore
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
