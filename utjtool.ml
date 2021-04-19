open Cmdliner

open Ututil
open Utio
open Utconv
open Utparse0
open Uttypecheck

module Convert = struct

let convert1 cc ~with_predefined ~verbose ~typecheck infile outfile =
  let stl = convert_file ~with_predefined cc infile in
  if typecheck then
    ignore (tc_structure TEnv.mt stl) ;
  let oc = if outfile = "-" then stdout else open_out outfile in
  if verbose then
    Fmt.(pf stderr "[convert %s to %s]\n%!" infile outfile) ;
  output_string oc (structure_to_string stl) ;
  output_string oc "\n" ;
  if outfile <> "-" then close_out oc

let convert_to_utj with_predefined verbose typecheck filepath destdir outputfile files =
  let filepath = List.concat filepath in
  let cc = CC.mk ~verbose ~filepath () in
  match (destdir, files, outputfile) with
    ("", [infile], "-") ->
    convert1 cc ~with_predefined ~verbose ~typecheck infile "-"

  | ("", [infile], outfile) when outfile <> "-" ->
    mkdir_p Fpath.(outfile |> v |> parent |> to_string) ;
    convert1 cc ~with_predefined ~verbose ~typecheck infile outfile

  | (destdir, infiles, "-") when destdir <> "" ->
    mkdir_p destdir ;
    infiles |> List.iter (fun infile ->
        if not Fpath.(infile |> v |> has_ext "json") then
          Fmt.(failwithf "input file %s is not JSON" infile) ;
        let outfile = Fpath.(to_string (append (v destdir) (infile |> v |> set_ext "utj" |> basename |> v))) in
        convert1 cc ~with_predefined ~verbose ~typecheck infile outfile
      )
  | _ -> Fmt.(failwithf "Bad args\nfilepath = %s\ndestdir = %s\noutputfile = %s\nfiles = %s\n"
                (String.concat ":" filepath)
                destdir
                outputfile
                (String.concat ", " files))

(* Command line interface *)

open Cmdliner

let cmd =
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE") in
  let destdir =
    let doc = "Output files to $(docv)." in
    Arg.(value & opt dir "" & info ["destdir";"d"]
           ~docv:"DIR" ~doc)
  in
  let outputfile =
    let doc = "Output file." in
    Arg.(value & opt string "-" & info ["output-file";"o"]
           ~docv:"FILE" ~doc)
  in
  let filepath =
    let open Cmdliner in
    let doc = "path for finding UTJ files" in
    let env = Arg.env_var "UTJPATH" ~doc in
    Arg.(value & opt_all (list ~sep:':' dir) [] & info ["utj-path"] ~env ~docv:"UTJ-PATH" ~doc) in

  let with_predefined =
    let doc = "Add import of predefined.utj." in
    Arg.(value & flag & info ["p"; "with-predefined"] ~doc) in

  let verbose =
    let doc = "Be verbose." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc) in

  let typecheck =
    let doc = "Also typecheck." in
    Arg.(value & flag & info ["t"; "typecheck"] ~doc) in

  let doc = "convert JSON to UTJ" in
  let man = [
    `S Manpage.s_description;
    `P "Convert JSON to UTJ."
  ]
  in
  Term.(const convert_to_utj $ with_predefined $ verbose $ typecheck $ filepath $ destdir $outputfile $ files),
  Term.info "convert" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man
end

module Typecheck = struct

let typecheck_utj verbose infiles =
  infiles |> List.iter (fun infile ->
      if not Fpath.(infile |> v |> has_ext "utj") then
        Fmt.(failwithf "file %s is not a UTJ" infile)
    ) ;
  infiles |> List.iter (fun infile ->
      let stl = parse_file parse_structure infile in
      ignore (tc_structure TEnv.mt stl)
    )

let cmd =
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE") in
  let verbose =
    let doc = "Be verbose." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc) in

  let doc = "Typecheck UTJ" in
  let man = [
    `S Manpage.s_description;
    `P "Typecheck UTJ."
  ]
  in
  Term.(const typecheck_utj $ verbose $ files),
  Term.info "typecheck" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man

end

module Help = struct

let help = ()

let cmd =
  let open Cmdliner in
  let doc = "UTJ tool" in
  let man = [
    `S Manpage.s_description;
    `P "UTJ Tool"
  ]
  in
  Term.(const help),
  Term.info "help" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man
end
;;

if not !Sys.interactive then
  Term.(exit @@ eval_choice Help.cmd [Help.cmd; Convert.cmd; Typecheck.cmd])
;;
