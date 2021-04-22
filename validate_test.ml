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

let validate_file ~filepath ~schema ~instance ?(tid=(None, ID.of_string "t")) () =
  let tdl = 
    FinalExtract.exec (full_extract (convert_file ~with_predefined:true (CC.mk ~filepath ()) schema)) in
  validate tdl (Yojson.Basic.from_file instance) tid

let validate_json ~filepath ~schema ~instance ?(tid=(None, ID.of_string "t")) () =
  let tdl = 
    FinalExtract.exec (full_extract (convert_file ~with_predefined:true (CC.mk ~filepath ()) schema)) in
  validate tdl (Yojson.Basic.from_string instance) tid

let success_file (schema, instance) =
  (schema^" || "^instance) >:: (fun ctxt ->
      assert_bool "should be true" (validate_file
                                      ~filepath:["schema-golden/schema-overrides"]
                                      ~schema
                                      ~instance
                                      ())
    )

let success_json (schema, instance) =
  (schema^" || "^instance) >:: (fun ctxt ->
      assert_bool "should be true" (validate_json
                                      ~filepath:["schema-golden/schema-overrides"]
                                      ~schema
                                      ~instance
                                      ())
    )

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ; "product0" >:: (fun ctxt ->
        let tdl = 
          let filepath = ["schema-golden/schema-overrides"] in
          let f = "schema-overrides/product-schema.json" in
          FinalExtract.exec (full_extract (convert_file ~with_predefined:true (CC.mk ~filepath ()) f)) in
        assert_bool "should be true" (validate tdl (Yojson.Basic.from_file "schema-overrides/product.json") (None, ID.of_string "t"))
      )
  ; "product1" >:: (fun ctxt ->
      assert_bool "should be true" (validate_file
                                      ~filepath:["schema-golden/schema-overrides"]
                                      ~schema:"schema-overrides/product-schema.json"
                                      ~instance:"schema-overrides/product.json"
                                      ())
    )
  ; success_file ("schema-overrides/product-schema.json","schema-overrides/product.json")
  ; success_json ("schema-overrides/product-schema.json",
{|
{
    "productId": 1,
    "productName": "An ice sculpture",
    "price": 12.50,
    "tags": [ "cold", "ice" ],
    "dimensions": {
      "length": 7.0,
      "width": 12.0,
      "height": 9.5
    },
    "warehouseLocation": {
      "lattitude": -78.75,
      "longitude": 20.4
    }
}
|})
  ]




let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
