exception Compilation_error of Lexing.position * string

type stop_after =
  | Stop_after_lex
  | Stop_after_parse
  | Stop_after_type
  | Stop_after_type_graph
  | Stop_after_codegen

let generate_output_filename (stop_after : stop_after) (input_filename : string) : string =
  let suffix = ".c" in
  let input_filename_without_suffix =
    if input_filename |> String.ends_with ~suffix
    then String.sub input_filename 0 (String.length input_filename - String.length suffix)
    else input_filename in
  match stop_after with
  | Stop_after_lex -> input_filename_without_suffix ^ ".lex.txt"
  | Stop_after_parse -> input_filename_without_suffix ^ ".parse.txt"
  | Stop_after_type -> input_filename_without_suffix ^ ".type.txt"
  | Stop_after_type_graph -> input_filename_without_suffix ^ ".type.dot"
  | Stop_after_codegen -> input_filename_without_suffix ^ ".s"

let wrap_lexbuf_errors (type a) (lexbuf : Lexing.lexbuf) (f : unit -> a) : a =
  try f () with
  | Lexer.Error error_text ->
    raise (Compilation_error (lexbuf |> Lexing.lexeme_start_p, "lexing error: " ^ error_text))
  | Parser.Error ->
    raise (Compilation_error (lexbuf |> Lexing.lexeme_start_p, "parsing error"))

let main ~(stop_after : stop_after) ~(input_filename : string) ~(output_filename : string) : unit =
  let output_channel = open_out_bin output_filename in
  let output_formatter = Format.formatter_of_out_channel output_channel in
  Format.pp_set_margin output_formatter 120;
  let lexbuf = Lexing.from_channel (open_in_bin input_filename) in
  Lexing.set_filename lexbuf input_filename;
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  if stop_after = Stop_after_lex then (
    let continue = ref true in
    while !continue do
      let position = lexbuf |> Lexing.lexeme_start_p in
      let token = wrap_lexbuf_errors lexbuf (fun () -> lexbuf |> Lexer.token) in
      Format.fprintf output_formatter "%a: %a@." Utils.pp_position position Lexer.pp_token token;
      if token = Lexer.EOF then continue := false
    done
  ) else (
    let file = wrap_lexbuf_errors lexbuf (fun () -> Parser.file Lexer.token lexbuf) in
    if stop_after = Stop_after_parse then
      Format.fprintf output_formatter "%a@." Ast.pp_file file
    else
      let program =
        try Typeck.typeck_ast_file file with
        | Typeck.Error (position, path, error_text) ->
          raise (Compilation_error (
            position,
            Format.asprintf "typeing error for `%a`:@ %s" Typed.pp_path path error_text
          )) in
      if stop_after = Stop_after_type then
        Format.fprintf output_formatter "%a@." Typed.pp_program program
      else if stop_after = Stop_after_type_graph then
        Typed.pp_program_graph output_formatter program
      else if stop_after = Stop_after_codegen then
        Format.pp_print_string output_formatter (Codegen.emit_program program)
  );
  close_out output_channel
