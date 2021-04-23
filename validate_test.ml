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

let validate_file ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(utype="t") ?(path="") () =
  let ut = of_string_exn utype in
  let tdl = 
    FinalExtract.exec (full_extract (load_and_convert (CC.mk ~filepath ()) schema)) in
  let j = Yojson.Basic.from_file instance in
  let j = traverse j path in
  validate tdl j ut

let validate_json ?(filepath=["schema-golden/schema-overrides";"utj-generated"]) ~schema ~instance ?(utype="t") ?(path="") () =
  let ut = of_string_exn utype in
  let tdl = 
    FinalExtract.exec (full_extract (load_and_convert (CC.mk ~filepath ()) schema)) in
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

let schema_test_pairs ~excluded_tests schema =
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
      |> List.filter (fun s -> not (List.mem s excluded_tests))
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
  ; success_file ("schema-overrides/cloud-sdk-pipeline-config-schema-FIXED.utj",
                  "schemastore/src/test/cloud-sdk-pipeline-config-schema/empty.json")
  ; success_file ("schema-overrides/cloud-sdk-pipeline-config-schema-FIXED.utj",
                  "schemastore/src/test/cloud-sdk-pipeline-config-schema/prodDeployment.json")
  ; success_file ("schemastore/src/schemas/json/bowerrc.json",
                  "schema-overrides/bowerrc-test2-FIXED.json")
  ; success_file ("schemastore/src/schemas/json/bootstraprc.json",
                  "schema-overrides/bootstraprc-test2-FIXED.json")
  ; success_file ("schemastore/src/schemas/json/bootstraprc.json",
                  "schema-overrides/bootstraprc-test3-FIXED.json")
  ; success_file ("schemastore/src/schemas/json/bootstraprc.json",
                  "schema-overrides/bootstraprc-test-FIXED.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-clearwater-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-compute-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-hosts-input-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-local-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-openstack-lamp.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-openvpn-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-playbook-compute-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-playbook-openstack-lamp.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/ansible-relationships-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-aks.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-app-sample-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-fortigate-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-grid-compute.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-linux-nodecellar.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-local-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-local-data-disks.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-network-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-vm.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/azure-windows-iis-loadbalanced.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/cloudify-test.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-ansible-container.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-ansible-container-using-docker-ansible-playbook.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-any-container.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-install-docker.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-mc-docker.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/docker-terraform-container-using-docker-terraform-module.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/helm-install-chart-using-interfaces.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/helm-install-chart-with-sa-and-kubeconfig-file-content.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-files-test-multiple-resources.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-file-test.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-file-test-persistent-volume.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-storage-class.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-test-deployment.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/k8s-test-stateful-set.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/openstack-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/openstack.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/terraform-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/terraform-wordpress-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utikities-ssh-key.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schema-overrides/utilities-cloudinit-aws.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-cloudinit-simple.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-config-simple.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-custom-workflow-example.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-deployment-proxy-custom-workflow.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-deployment-proxy.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-deployment-without-workflow.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-file-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-file-openstack-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-ftp-upload_ftp.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-hook-workflow-check-failure.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-node-instance-proxy.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-rest.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-scalelist.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-secrets-ead-secret-blueprint.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-suspend.json")
  ; success_file ("schema-overrides/cloudify-FIXED.utj",
                  "schemastore/src/test/cloudify/utilities-terminal-cisco.json")

  ; success_file ("schema-overrides/bukkit-plugin-FIXED.utj",
                  "schemastore/src/test/bukkit-plugin/bukkit-plugin-test.json")
]

let excluded_schema = [
  "ansible-inventory.json"
; "azure-iot-edge-deployment-template-2.0.json"
; "azure-iot-edge-deployment-template-1.0.json"
; "cloud-sdk-pipeline-config-schema.json"
; "bukkit-plugin.json"
; "cloudify.json"
]

let excluded_tests = [
  "schemastore/src/test/bowerrc/bowerrc-test2.json"
; "schemastore/src/test/bootstraprc/bootstraprc-test2.json"
; "schemastore/src/test/bootstraprc/bootstraprc-test3.json"
; "schemastore/src/test/bootstraprc/bootstraprc-test.json"
; "schemastore/src/test/cloudify/azure-aks.json"
; "schemastore/src/test/cloudify/azure-windows-iis-loadbalanced.json"
; "schemastore/src/test/cloudify/utilities-cloudinit-aws.json"
; "schemastore/src/test/cloudify/openstack.json"
; "schemastore/src/test/cloudify/openstack-blueprint.json"
]

let testfiles = subtract all_schemastore_files excluded_schema
let testfiles = (firstn 46 testfiles)
let schemastore = "schemastore" >::: (
   testfiles |> List.concat_map (schema_test_pairs ~excluded_tests) |> List.map success_file
  )

let tests = "all" >::: [
    simple
  ; exceptions
  ; schemastore
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
