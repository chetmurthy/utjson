open Cmdliner

(* Implementation of the command, we just print the args. *)

let convert_to_utj filepath destdir files =
  let filepath = List.concat filepath in
  Printf.printf "filepath = %s\ndestdir = %s\nfiles = %s\n"
    (String.concat ":" filepath)
    destdir (String.concat ", " files)

(* Command line interface *)

open Cmdliner

let convert_cmd =
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE") in
  let destdir =
    let doc = "Output files to $(docv)." in
    Arg.(value & opt dir Filename.current_dir_name & info ["destdir";"o"]
           ~docv:"DIR" ~doc)
  in
  let filepath =
    let open Cmdliner in
    let doc = "path for finding UTJ files" in
    let env = Arg.env_var "UTJPATH" ~doc in
    Arg.(value & opt_all (list ~sep:':' dir) [] & info ["utj-path"] ~env ~docv:"UTJ-PATH" ~doc) in

  let doc = "convert JSON to UTJ" in
  let man = [
    `S Manpage.s_description;
    `P "Convert JSON to UTJ."
  ]
  in
  Term.(const convert_to_utj $ filepath $ destdir $ files),
  Term.info "convert" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man

let help = ()

let help_cmd =
  let open Cmdliner in
  let doc = "UTJ tool" in
  let man = [
    `S Manpage.s_description;
    `P "UTJ Tool"
  ]
  in
  Term.(const help),
  Term.info "help" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval_choice help_cmd [help_cmd; convert_cmd])
