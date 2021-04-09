open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Utypes
open Utparse0
open Utprint
open Utconv

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp (Str.quote pattern)) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern ?msg pattern f =
  Testutil.assert_raises_exn_pred ?msg
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

let of_string_exn s = s |> parse_string parse_utype_eoi
let to_string t = print_utype Pprintf.empty_pc t

let struct_item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let struct_item_to_string t = print_struct_item Pprintf.empty_pc t

let sig_item_of_string_exn s = s |> parse_string parse_sig_item_eoi
let sig_item_to_string t = print_sig_item Pprintf.empty_pc t

let module_expr_of_string_exn s = s |> parse_string parse_module_expr_eoi
let module_expr_to_string t = print_module_expr Pprintf.empty_pc t

let module_type_of_string_exn s = s |> parse_string parse_module_type_eoi
let module_type_to_string t = print_module_type Pprintf.empty_pc t

let printer = show_utype_t
let cmp = equal_utype_t

let struct_item_printer x = "<<"^(show_struct_item_t x)^">>"
let struct_item_cmp = equal_struct_item_t


let module_expr_printer x = "<<"^(show_module_expr_t x)^">>"
let module_expr_cmp = equal_module_expr_t

let module_type_printer x = "<<"^(show_module_type_t x)^">>"
let module_type_cmp = equal_module_type_t

let sig_item_printer x = "<<"^(show_sig_item_t x)^">>"
let sig_item_cmp = equal_sig_item_t

let item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let item_to_string t = print_struct_item Pprintf.empty_pc t
let item_printer x = "<<"^(show_struct_item_t x)^">>"
let item_cmp = equal_struct_item_t

let success (expect, f) =
  assert_equal ~msg:f ~printer:item_printer ~cmp:item_cmp
    (item_of_string_exn expect)
    (load_file f)

let successf (expectf, f) =
  assert_equal ~msg:f ~printer:item_printer ~cmp:item_cmp
    (load_file expectf)
    (load_file f)

let convert1 f =
  let fp = "schemastore/src/schemas/json/"^f in
  load_file fp

let convert_check1 f =
  f >:: (fun ctxt ->
      ignore(convert1 f)
    )

let convert_check = "convert_check" >::: (List.map convert_check1 [
    "ansible-inventory.json"
(*
  ;"ansible-playbook.json"
*)
(* BUG in schema
  ;"ansible-role-2.0.json"
  ;"ansible-role-2.1.json"
  ;"ansible-role-2.2.json"
  ;"ansible-role-2.3.json"
  ;"ansible-role-2.4.json"
  ;"ansible-role-2.5.json"
  ;"ansible-role-2.6.json"
  ;"ansible-role-2.7.json"
  ;"ansible-role-2.9.json"
*)
(* OVERRIDDEN
  ;"apibuilder.json"
*)
  ;"apple-app-site-association.json"
(* OVERRIDDEN
  ;"appsettings.json"
*)
  ;"appsscript.json"
  ;"appveyor.json"
  ;"asmdef.json"
  ;"avro-avsc.json"
  ;"azure-iot-edgeagent-deployment-1.0.json"
  ;"azure-iot-edgeagent-deployment-1.1.json"
  ;"azure-iot-edge-deployment-1.0.json"
  ;"azure-iot-edge-deployment-2.0.json"
  ;"azure-iot-edge-deployment-template-1.0.json"
  ;"azure-iot-edge-deployment-template-2.0.json"
  ;"azure-iot-edgehub-deployment-1.0.json"
  ;"azure-iot-edgehub-deployment-1.1.json"
  ;"azure-iot-edgehub-deployment-1.2.json"
  ;"babelrc.json"
  ;"backportrc.json"
  ;"bamboo-spec.json"
  ;"band-manifest.json"
  ;"behat.json"
  ;"bitrise.json"
  ;"bitrise-step.json"
  ;"BizTalkServerApplicationSchema.json"
  ;"bootstraprc.json"
  ;"bower.json"
  ;"bowerrc.json"
  ;"bozr.json"
  ;"bukkit-plugin.json"
  ;"bundleconfig.json"
  ;"bungee-plugin.json"
  ;"chrome-manifest.json"
  ;"chutzpah.json"
(* malformed, using JSON as structure
  ;"circleciconfig.json"
*)
(* "fileMatch" member at toplevel
  ;"cirrus.json"
*)
  ;"clasp.json"
  ;"cloudbuild.json"
(* properties are listed but not in "properties" (e.g. "valid_values")
  ;"cloudify.json"
*)
(* properties are listed but not in "properties"
  ;"cloud-sdk-pipeline-config-schema.json"
*)
(* properties are listed but not in "properties"
  ;"codecov.json"
*)
  ;"codeship-services.json"
  ;"codeship-steps.json"
  ;"coffeelint.json"
  ;"comet.json"
  ;"commands.json"
  ;"commitlintrc.json"
  ;"compile-commands.json"
  ;"compilerconfig.json"
  ;"compilerdefaults.json"
  ;"component.json"
  ;"composer.json"
  ;"config.json"
  ;"container-structure-test.json"
  ;"content-security-policy-report-2.json"
  ;"cosmos-config.json"
(* superfluous "scope" member as part of some types
  ;"creatomic.json"
*)
  ;"cryproj.52.schema.json"
  ;"cryproj.53.schema.json"
  ;"cryproj.54.schema.json"
  ;"cryproj.55.schema.json"
  ;"cryproj.dev.schema.json"
  ;"cryproj.json"
  ;"csscomb.json"
  ;"csslintrc.json"
(* properties are listed but not in "properties"
  ;"dart-build.json"
*)
  ;"dart-test.json"
  ;"datalogic-scan2deploy-android.json"
  ;"datalogic-scan2deploy-ce.json"
  ;"debugsettings.json"
(* extra members from intellij ?
  ;"dependabot-2.0.json"
*)
  ;"dependabot.json"
  ;"detekt.json"
  ;"devinit.schema-1.0.json"
  ;"devinit.schema-2.0.json"
  ;"devinit.schema-3.0.json"
  ;"devinit.schema-4.0.json"
  ;"devinit.schema-5.0.json"
  ;"devinit.schema-6.0.json"
  ;"docfx.json"
  ;"dotnetcli.host.json"
  ;"drone.json"
(* "titles", probably a typo from "title"
  ;"drush.site.yml.json"
*)
(* "$xsd-type", other extraneous keys
  ;"dss-2.0.0.json"
*)
(* "typeof" ?  wazzat?
  ;"electron-builder.json"
*)
  ;"epr-manifest.json"
  ;"eslintrc.json"
  ;"esmrc.json"
  ;"esquio.json"
(* "fallback" instead of "default" ?
  ;"expo-37.0.0.json"
*)
  ;"expo-38.0.0.json"
  ;"expo-39.0.0.json"
  ;"expo-40.0.0.json"
  ;"fabric.mod.json"
  ;"feed-1.json"
  ;"feed.json"
  ;"foxx-manifest.json"
  ;"function.json"
  ;"geojson.json"
  ;"github-action.json"
  ;"github-funding.json"
  ;"github-workflow.json"
  ;"gitlab-ci.json"
  ;"gitversion.json"
  ;"global.json"
  ;"golangci-lint.json"
  ;"grafana-dashboard-5.x.json"
  ;"grunt-clean-task.json"
  ;"grunt-copy-task.json"
  ;"grunt-cssmin-task.json"
  ;"grunt-jshint-task.json"
  ;"grunt-task.json"
  ;"grunt-watch-task.json"
  ;"hadolint.json"
  ;"haxelib.json"
(* intellij
  ;"helmfile.json"
*)
  ;"hemtt-0.6.2.json"
  ;"host.json"
  ;"host-meta.json"
  ;"htmlhint.json"
  ;"httpmockrc.json"
(* intellij
  ;"huskyrc.json"
*)
  ;"imageoptimizer.json"
  ;"install.json"
  ;"io-package.json"
(* "links" ?
  ;"jasonette.json"
*)
  ;"jdt.json"
  ;"jekyll.json"
  ;"jovo-language-model.json"
  ;"jsbeautifyrc.json"
  ;"jsbeautifyrc-nested.json"
(* extraneous definition -- should be an annotatiion
  ;"jsconfig.json"
*)
  ;"jscsrc.json"
  ;"jsdoc-1.0.0.json"
  ;"jshintrc.json"
  ;"jsinspectrc.json"
  ;"json-api-1.0.json"
  ;"jsone.json"
  ;"jsonld.json"
  ;"json-patch.json"
(* properties are listed but not in "properties"
  ;"ksp-avc.json"
*)
  ;"ksp-ckan.json"
  ;"kustomization.json"
  ;"launchsettings.json"
  ;"lerna.json"
  ;"libman.json"
(* just definitions, no actual type
  ;"licenses.1.json"
*)
  ;"lintstagedrc.schema.json"
  ;"local.settings.json"
  ;"lsdlschema-0.7.json"
  ;"lsdlschema-1.0.json"
  ;"lsdlschema-1.2.json"
  ;"lsdlschema-2.0.json"
  ;"lsdlschema-3.0.json"
  ;"lsdlschema-3.1.json"
  ;"lsdlschema.json"
  ;"mimetypes.json"
  ;"mocharc.json"
  ;"modernizrrc.json"
  ;"mtad.json"
  ;"mtaext.json"
  ;"mta.json"
  ;"mycode.json"
(* "fileMatch" member at toplevel
  ;"neoload.json"
*)
  ;"nest-cli.json"
  ;"netlify.json"
  ;"nightwatch.json"
(* "required" as a boolean member, not a list of fields
  ;"ninjs-1.0.json"
*)
  ;"ninjs-1.1.json"
  ;"ninjs-1.2.json"
  ;"ninjs-1.3.json"
  ;"nodehawkrc.json"
(* malformed schema in dependencies/nodeArgs
  ;"nodemon.json"
*)
  ;"now.json"
  ;"npm-link-up.json"
  ;"npmpackagejsonlintrc.json"
  ;"nswag.json"
  ;"nuget-project-3.3.0.json"
  ;"nuget-project.json"
  ;"ocelot.json"
  ;"omnisharp.json"
  ;"openfin.json"
  ;"openweather.current.json"
  ;"openweather.roadrisk.json"
  ;"opspec-io-0.1.7.json"
(* "tsType" ?
  ;"package.json"
*)
  ;"package.manifest-7.0.0.json"
  ;"package.manifest-8.0.0.json"
  ;"package.manifest.json"
  ;"packer.json"
  ;"pattern.json"
  ;"pgap_yaml_input_reader.json"
  ;"phraseapp.json"
  ;"pm2-ecosystem.json"
  ;"pocketmine-plugin.json"
  ;"pre-commit-config.json"
  ;"prettierrc-1.8.2.json"
  ;"prettierrc.json"
  ;"prisma.json"
  ;"project-1.0.0-beta3.json"
  ;"project-1.0.0-beta4.json"
  ;"project-1.0.0-beta5.json"
  ;"project-1.0.0-beta6.json"
  ;"project-1.0.0-beta8.json"
  ;"project-1.0.0-rc1.json"
  ;"project-1.0.0-rc2.json"
  ;"project.json"
  ;"prometheus.json"
  ;"prometheus.rules.json"
(* "defaultSnippets" -- is this like "examples" ?
  ;"proxies.json"
*)
  ;"pubspec.json"
  ;"pyrseas-0.8.json"
  ;"renovate.json"
  ;"resjson.json"
  ;"resume.json"
  ;"roadrunner.json"
  ;"sarif-1.0.0.json"
  ;"sarif-2.0.0-csd.2.beta.2018-10-10.json"
  ;"sarif-2.0.0-csd.2.beta.2019-01-09.json"
  ;"sarif-2.0.0-csd.2.beta.2019-01-24.json"
  ;"sarif-2.0.0.json"
  ;"sarif-2.1.0.json"
  ;"sarif-2.1.0-rtm.0.json"
  ;"sarif-2.1.0-rtm.1.json"
  ;"sarif-2.1.0-rtm.2.json"
  ;"sarif-2.1.0-rtm.3.json"
  ;"sarif-2.1.0-rtm.4.json"
  ;"sarif-2.1.0-rtm.5.json"
  ;"sarif-external-property-file-2.1.0.json"
  ;"sarif-external-property-file-2.1.0-rtm.0.json"
  ;"sarif-external-property-file-2.1.0-rtm.1.json"
  ;"sarif-external-property-file-2.1.0-rtm.2.json"
  ;"sarif-external-property-file-2.1.0-rtm.3.json"
  ;"sarif-external-property-file-2.1.0-rtm.4.json"
  ;"sarif-external-property-file-2.1.0-rtm.5.json"
  ;"sarif-external-property-file.json"
  ;"sarif.json"
  ;"schema-catalog.json"
(* exclusiveMinimum old style
  ;"schema-draft-v4.json"
*)
  ;"schema-org-action.json"
  ;"schema-org-contact-point.json"
  ;"schema-org-place.json"
  ;"schema-org-thing.json"
  ;"semgrep.json"
(* properties are listed but not in "properties"
  ;"servicehub.config.schema.json"
*)
  ;"servicehub.service.schema.json"
  ;"settings.job.json"
  ;"solidaritySchema.json"
  ;"sourcehut-build-0.41.2.json"
  ;"sourcehut-build-0.65.0.json"
  ;"sourcemap-v3.json"
  ;"specif-1.0.json"
  ;"sponge-mixins.json"
  ;"sprite.json"
  ;"staticwebapp.config.json"
  ;"stylelintrc.json"
  ;"stylintrc.json"
  ;"swagger-2.0.json"
  ;"swcrc.json"
(* definitions foolishness
  ;"taskfile.json"
*)
  ;"taurus.json"
  ;"template.json"
  ;"templatesources.json"
(* "@comment"
  ;"toolinfo.1.1.0.json"
*)
  ;"traefik-v2-file-provider.json"
  ;"traefik-v2.json"
(* definition fuckery
  ;"travis.json"
*)
(* more "//" with a bunch of documentation
  ;"tsconfig.json"
*)
  ;"tsd.json"
  ;"tsdrc.json"
  ;"ts-force-config.json"
(* based on outdated draft
  ;"tslint.json"
*)
  ;"tsoa.json"
  ;"typewiz.json"
  ;"typings.json"
  ;"typingsrc.json"
(* properties are listed but not in "properties"
  ;"ui5-manifest.json"
*)
  ;"up.json"
(* looks like based on outdated schema draft
  ;"vega.json"
*)
  ;"vega-lite.json"
  ;"vim-addon-info.json"
  ;"vs-2017.3.host.json"
  ;"vsconfig.json"
  ;"vsext.json"
  ;"vsix-manifestinjection.json"
  ;"vsix-publish.json"
  ;"vsls.json"
  ;"vs-nesting.json"
  ;"vss-extension.json"
  ;"webextension.json"
  ;"webjob-publish-settings.json"
  ;"webjobs-list.json"
  ;"web-manifest-app-info.json"
  ;"web-manifest-combined.json"
  ;"web-manifest.json"
  ;"web-types.json"
  ;"winget-pkgs.json"
  ;"workflows.json"
  ;"xs-app.json"
  ;"xunit.runner.schema.json"
  ])

let tests = "all" >::: [
    convert_check
]

if not !Sys.interactive then
  run_test_tt_main tests
;;