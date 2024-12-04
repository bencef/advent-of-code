{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | '\n' { new_line lexbuf; SEPARATOR }
  | [ 'a' - 'z' 'A' - 'Z'  ] { ITEM (String.get (lexeme lexbuf) 0) }
  | eof  { EOF }
