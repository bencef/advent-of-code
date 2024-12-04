{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | [ 'a' - 'z' ]  { CHAR(lexeme_char lexbuf 0) }
  | '\n'           { new_line lexbuf; read lexbuf }
  | _              { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof            { EOF }
