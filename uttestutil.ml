open OUnit2
open OUnitTest
open Pa_ppx_testutils

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

let select_schema_file f =
  let store_fp = "schemastore/src/schemas/json/"^f in
  let override_fp = "schema-overrides/"^f in
  let httpcache_fp = "http-schema-cache/"^f in
  if override_fp |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then
    override_fp
  else if httpcache_fp |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then
    httpcache_fp
  else 
    store_fp

let select_test_file schema f =
  let store_fp = "schemastore/src/test/"^schema^"/"^f in
  let override_fp = "schema-overrides/"^f in
  if override_fp |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then
    override_fp
  else 
    store_fp

let all_schemastore_files = [
  "kubernetesjsonschema.dev/master/_definitions.json"
; "ansible-inventory.json"
; "ansible-playbook.json"
;"ansible-role-2.0.json"
;"ansible-role-2.1.json"
;"ansible-role-2.2.json"
;"ansible-role-2.3.json"
;"ansible-role-2.4.json"
;"ansible-role-2.5.json"
;"ansible-role-2.6.json"
;"ansible-role-2.7.json"
;"ansible-role-2.9.json"
; "apibuilder.json"
;"apple-app-site-association.json"
;"appsettings.json"
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
;"circleciconfig.json"
;"cirrus.json"
;"clasp.json"
;"cloudbuild.json"
;"cloudify.json"
;"cloud-sdk-pipeline-config-schema.json"
;"codecov.json"
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
;"creatomic.json"
;"cryproj.52.schema.json"
;"cryproj.53.schema.json"
;"cryproj.54.schema.json"
;"cryproj.55.schema.json"
;"cryproj.dev.schema.json"
;"cryproj.json"
;"csscomb.json"
;"csslintrc.json"

;"dart-build.json"
;"dart-test.json"
;"datalogic-scan2deploy-android.json"
;"datalogic-scan2deploy-ce.json"
;"debugsettings.json"
;"dependabot-2.0.json"
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
;"drush.site.yml.json"
;"dss-2.0.0.json"
(* "typeof" ?  wazzat?
   ;"electron-builder.json"
*)
;"epr-manifest.json"
;"eslintrc.json"
;"esmrc.json"
;"esquio.json"
;"expo-37.0.0.json"
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
;"helmfile.json"
;"hemtt-0.6.2.json"
;"host.json"
;"host-meta.json"
;"htmlhint.json"
;"httpmockrc.json"
;"huskyrc.json"
;"imageoptimizer.json"
;"install.json"
;"io-package.json"
;"jasonette.json"
;"jdt.json"
;"jekyll.json"
;"jovo-language-model.json"
;"jsbeautifyrc.json"
;"jsbeautifyrc-nested.json"
;"jsconfig.json"
;"jscsrc.json"
;"jsdoc-1.0.0.json"
;"jshintrc.json"
;"jsinspectrc.json"
;"json-api-1.0.json"
;"jsone.json"
;"jsonld.json"
;"json-patch.json"
;"ksp-avc.json"
;"ksp-ckan.json"
;"kustomization.json"
;"launchsettings.json"
;"lerna.json"
;"libman.json"
;"licenses.1.json"
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
;"neoload.json"
;"nest-cli.json"
;"netlify.json"
;"nightwatch.json"
;"ninjs-1.0.json"
;"ninjs-1.1.json"
;"ninjs-1.2.json"
;"ninjs-1.3.json"
;"nodehawkrc.json"
;"nodemon.json"
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
;"package.json"
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
;"proxies.json"
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
;"schema-draft-v4.json"
;"schema-org-action.json"
;"schema-org-contact-point.json"
;"schema-org-place.json"
;"schema-org-thing.json"
;"semgrep.json"
;"servicehub.config.schema.json"
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
;"taskfile.json"
;"taurus.json"
;"template.json"
;"templatesources.json"
;"toolinfo.1.1.0.json"
;"traefik-v2-file-provider.json"
;"traefik-v2.json"
;"travis.json"
;"tsconfig.json"
;"tsd.json"
;"tsdrc.json"
;"ts-force-config.json"
;"tslint.json"
;"tsoa.json"
;"typewiz.json"
;"typings.json"
;"typingsrc.json"
;"ui5-manifest.json"
;"up.json"
;"vega.json"
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
]
