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
  ; success_file ("schema-overrides/ansible-role-2.9.json","schemastore/src/test/ansible-role-2.9/apt.json")
  ; success_file ("schema-overrides/ansible-role-2.9.json","schemastore/src/test/ansible-role-2.9/copy.json")
  ; success_file ("schema-overrides/apibuilder.json","schemastore/src/test/apibuilder/apibuilder-api.json")
  ; success_file ("schemastore/src/schemas/json/apple-app-site-association.json",
                  "schemastore/src/test/apple-app-site-association/apple-app-site-association_getting-started.json")
  ; success_file ("schema-overrides/appsettings.json","schemastore/src/test/appsettings/nlog.json")
  ; success_file ("schema-overrides/appsettings.json","schemastore/src/test/appsettings/umbraco.json")
  ; success_file ("schema-overrides/appsettings.json","schemastore/src/test/appsettings/umbraco-more-settings.json")
  ; success_file ("schema-overrides/appsettings.json","schemastore/src/test/appsettings/weboptimizer.json")
  ; success_file ("schema-overrides/appveyor.json",
                  "schemastore/src/test/appveyor/appveyor-matrix-config.json")
  ; success_file ("schema-overrides/appveyor.json",
                  "schemastore/src/test/appveyor/reference.json")
  ; success_file ("schemastore/src/schemas/json/asmdef.json",
                  "schemastore/src/test/asmdef/test01.asmdef.json")
  ; success_file ("schemastore/src/schemas/json/asmdef.json",
                  "schemastore/src/test/asmdef/test02.asmdef.json")
  ; success_file ("schemastore/src/schemas/json/asmdef.json",
                  "schemastore/src/test/asmdef/test03.asmdef.json")
  ; success_file ("schemastore/src/schemas/json/avro-avsc.json",
                  "schemastore/src/test/avro-avsc/sample.avsc.json")
  ]




let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
