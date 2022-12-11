let read_all (channel : in_channel) : string =
  let chunks = ref [] in
  let continue = ref true in
  while !continue do
    let chunk = Bytes.create 1024 in
    let read = input channel chunk 0 (chunk |> Bytes.length) in
    chunks := !chunks @ [Bytes.sub chunk 0 read];
    if read = 0 then continue := false
  done;
  !chunks |> Bytes.concat Bytes.empty |> String.of_bytes

let pp_position (formatter : Format.formatter) (position : Lexing.position) : unit =
  let c = position.pos_cnum - position.pos_bol + 1 in
  Format.fprintf formatter "File \"%s\", line %d, characters %d-%d" position.pos_fname position.pos_lnum (c - 1) c

let show_typ (position : Lexing.position) : string =
  Format.asprintf "%a" pp_position position
