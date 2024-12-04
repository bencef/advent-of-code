{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | _              { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof            { EOF }
