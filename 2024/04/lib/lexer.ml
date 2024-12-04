open Sedlexing.Utf8
open Parser

exception Unknown_token of string

let token buf =
  match%sedlex buf with
  | eof -> EOF
  | '\n' -> NEWLINE
  | 'X' -> X
  | 'M' -> M
  | 'A' -> A
  | 'S' -> S
  | _ -> Unknown_token (lexeme buf) |> raise
