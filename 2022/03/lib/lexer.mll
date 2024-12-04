{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | '\n' { new_line lexbuf; read lexbuf }
  | [ 'a' - 'z' 'A' - 'Z'  ] + { ITEM (lexeme lexbuf) }
  | eof  { EOF }
