{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | '\n'            { new_line lexbuf; read lexbuf}
  | ','             { COMMA }
  | '-'             { DASH }
  | [ '0' - '9' ] + { ID (lexeme lexbuf |> Int.of_string) }
  | eof             { EOF }
