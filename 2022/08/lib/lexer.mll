{
  open! Core
  open! Lexing
  open Tokens
}

rule read =
  parse
  | [ '0' - '9' ] + { DIGIT(lexeme lexbuf |> int_of_string) }
  | '\n'            { new_line lexbuf; LINE_END }
  | _               { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof             { EOF }
