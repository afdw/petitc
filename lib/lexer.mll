{
  exception Error of string

  type token = [%import: Parser.token] [@@deriving show { with_path = false }]
}

rule token = parse
  | " " | "\t" { lexbuf |> token }
  | "\n" { lexbuf |> Lexing.new_line; lexbuf |> token }
  | "//" [^ '\n']* "\n" { lexbuf |> Lexing.new_line; lexbuf |> token }
  | "//" [^ '\n']* eof { EOF }
  | "/*" { lexbuf |> comment }
  | eof { EOF }
  | (['a' - 'z' 'A' - 'Z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*) as s {
    match s with
    | "bool" -> BOOL
    | "break" -> BREAK
    | "continue" -> CONTINUE
    | "else" -> ELSE
    | "false" -> FALSE
    | "for" -> FOR
    | "if" -> IF
    | "int" -> INT
    | "NULL" -> NULL
    | "return" -> RETURN
    | "sizeof" -> SIZEOF
    | "true" -> TRUE
    | "void" -> VOID
    | "while" -> WHILE
    | _ -> IDENT s
  }
  | ('0' | (['1' - '9'] ['0' - '9']*)) as s { LIT (Int64.of_string s) }
  | "'" ([^ '\\' '\''] as c) "'" { LIT (int_of_char c |> Int64.of_int) }
  | "'\\\\'" { LIT (int_of_char '\\' |> Int64.of_int) }
  | "'\\''" { LIT (int_of_char '\'' |> Int64.of_int) }
  | "'\\n'" { LIT (int_of_char '\n' |> Int64.of_int) }
  | "'\\t'" { LIT (int_of_char '\t' |> Int64.of_int) }
  | "#include" " "* "<" ([^ '>' '\n']* as s) ">\n" { lexbuf |> Lexing.new_line; INCLUDE s }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "++" { PLUS_PLUS }
  | "+" { PLUS }
  | "--" { MINUS_MINUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "&&" { AMPERSAND_AMPERSAND }
  | "&" { AMPERSAND }
  | "||" { BAR_BAR }
  | "!=" { EXCLAMATION_EQUALS }
  | "!" { EXCLAMATION }
  | "==" { EQUALS_EQUALS }
  | "=" { EQUALS }
  | "<=" { LESS_EQUALS }
  | "<" { LESS }
  | ">=" { GREATER_EQUALS }
  | ">" { GREATER }
  | _ as c { raise (Error (Printf.sprintf "illegal character: '%c'" c)) }

and comment = parse
  | "\n" { lexbuf |> Lexing.new_line; lexbuf |> comment }
  | "*/" { lexbuf |> token }
  | _ { lexbuf |> comment }
  | eof { raise (Error "unclosed comment") }
