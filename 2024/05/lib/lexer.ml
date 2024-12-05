open Sedlexing.Utf8
open Parser

exception Unknown_token of string

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let separator = [%sedlex.regexp? "\n\n"]

let rec token buf =
  match%sedlex buf with
  | eof -> EOF
  | separator -> SEPARATOR
  | '|' -> BEFORE
  | ',' -> COMMA
  | number -> NUMBER (lexeme buf |> int_of_string)
  | Plus white_space -> token buf
  | _ -> Unknown_token (lexeme buf) |> raise
