open OUnit2
open OUnitTest
open Pa_ppx_utils.Std
open Pa_ppx_testutils
open Uttestutil

open Utypes
open Utparse0
open Utprint
open Ututil
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

let traverse j paths =
  if paths = "" then j else
  traverse_json j (String.split_on_char '/' paths)

let load_and_extract ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) schema =
  FinalExtract.exec (full_extract (load_and_convert (CC.mk ~filepath ()) schema))

let validate_file ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(utype="t") ?(path="") () =
  let ut = of_string_exn utype in
  let tdl = load_and_extract ~filepath schema in
  let j = Yojson.Basic.from_file instance in
  let j = traverse j path in
  validate tdl j ut

let validate_json ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(utype="t") ?(path="") () =
  let ut = of_string_exn utype in
  let tdl = load_and_extract ~filepath schema in
  let j = Yojson.Basic.from_string instance in
  let j = traverse j path in
  validate tdl j ut

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

let schemaroot = Fpath.("schemastore/src/schemas/json" |> v)
let testroot = Fpath.("schemastore/src/test" |> v)
let tests_for_schema schema =
  assert (not Fpath.(has_ext "json" schema || has_ext "utj" schema)) ;
  let testdir = Fpath.(append testroot schema) in
  let l =
    if testdir |> Bos.OS.Dir.exists |> Rresult.R.get_ok then
      testdir
      |> Bos.OS.Dir.contents ~rel:true
      |> Rresult.R.get_ok
    else []
  in
  (schema,l)

let schema_path = [Fpath.v "schema-overrides/FIXED-schema"; schemaroot]
let test_path = [Fpath.v "schema-overrides/FIXED-tests"; testroot]

let find_schema schema =
  assert (not Fpath.(has_ext "json" schema || has_ext "utj" schema)) ;
  match schema_path |> List.find_map (fun dir ->
      let jsonf = Fpath.(append dir (add_ext "json" schema)) in
      let utjf = Fpath.(append dir (add_ext "utj" schema)) in
      if utjf |> Bos.OS.File.exists |> Rresult.R.get_ok then Some utjf
      else if jsonf |> Bos.OS.File.exists |> Rresult.R.get_ok then Some jsonf
      else None) with
    Some f -> f
  | None -> Fmt.(failwith "find_schema: cannot find schema file %a" Fpath.pp schema)

let find_test schema test =
  assert (not Fpath.(has_ext "json" schema || has_ext "utj" schema)) ;
  assert Fpath.(has_ext "json" test) ;
  match test_path |> List.find_map (fun dir ->
      let testf = Fpath.(append (append dir schema) test) in
      if testf |> Bos.OS.File.exists |> Rresult.R.get_ok then Some testf
      else None) with
    Some f -> f
  | None -> Fmt.(failwith "find_test: cannot find test file %a/%a" Fpath.pp schema Fpath.pp test)

let schema_test_files schemabase =
  let schema = Fpath.(rem_ext schemabase) in
  let (_, tests) = tests_for_schema schema in
  let schemafile = find_schema schema in
  let testfiles = List.map (find_test schema) tests in
  (schemafile, testfiles)

let schema_test schemabase =
  let (schemafile, testfiles) = schema_test_files schemabase in
  (Fpath.to_string schemabase) >::: (testfiles |> List.map (fun t ->
      success_file (Fpath.to_string schemafile, Fpath.to_string t)))


let testfiles = firstn 100 all_schemastore_files
let testfiles = subtract testfiles [
    "kubernetesjsonschema.dev/master/_definitions.json"
  ; "jsonld.json"
  ; "jsone.json"
  ]
let schemastore = "schemastore" >:::
                   (testfiles
                    |> List.map Fpath.v
                    |> List.map schema_test)

let tests = "all" >::: [
    schemastore
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
