open Main

let () =
  let stop_after = ref Stop_after_codegen in
  let speclist = [
    ("--lex-only", Arg.Unit (fun () -> stop_after := Stop_after_lex), "Stop after lexing");
    ("--parse-only", Arg.Unit (fun () -> stop_after := Stop_after_parse), "Stop after parsing");
    ("--type-only", Arg.Unit (fun () -> stop_after := Stop_after_type), "Stop after typing");
  ] in
  let input_filenames = ref [] in
  Arg.parse speclist (fun input_filename -> input_filenames := !input_filenames @ [input_filename]) "petitc [options] file";
  if !input_filenames |> List.length <> 1 then (
    Format.eprintf "Usage error: exactly one input file required@.";
    exit 1
  );
  let input_filename = !input_filenames |> List.hd in
  let output_filename = Main.generate_output_filename !stop_after input_filename in
  try main ~stop_after:!stop_after ~input_filename ~output_filename with
  | Compilation_error (position, error_text) ->
    Format.eprintf "%a:@." Utils.pp_position position;
    Format.eprintf "Compilation error: %s@." error_text;
    exit 1
  | Sys_error error_text ->
    Format.eprintf "System error: %s@." error_text;
    exit 1
