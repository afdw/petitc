open Main

let test_compilation ~(stop_after : stop_after) ~(input_filename : string) : unit =
  let expected_output_filename = generate_output_filename stop_after input_filename in
  let actual_output_filename = Filename.temp_file "test_compilation." ".out" in
  main ~stop_after ~input_filename ~output_filename:actual_output_filename;
  let expected_output = Utils.read_all (open_in_bin expected_output_filename) in
  let actual_output = Utils.read_all (open_in_bin actual_output_filename) in
  assert (
    if actual_output <> expected_output then (
      Printf.printf "Expected (length %d):\n" (expected_output |> String.length);
      Printf.printf "%s" expected_output;
      Printf.printf "Actual (length %d):\n" (actual_output |> String.length);
      Printf.printf "%s" actual_output
    );
    actual_output = expected_output
  )

let () =
  test_compilation ~stop_after:Stop_after_lex ~input_filename:"../../../test/comments.c"

let () =
  try
    test_compilation ~stop_after:Stop_after_lex ~input_filename:"../../../test/unclosed_comment.c";
    failwith "error expected"
  with
  | Compilation_error(_, "lexing error: unclosed comment") -> ()

let () =
  test_compilation ~stop_after:Stop_after_lex ~input_filename:"../../../test/tokens.c"

let () =
  test_compilation ~stop_after:Stop_after_parse ~input_filename:"../../../test/parsing.c"
