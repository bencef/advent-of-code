open Sedlexing.Utf8
open Parser

exception Unknown_token of string

let digit = [%sedlex.regexp? '0' .. '9']

let rec token buf =
  match%sedlex buf with
  | eof -> EOF
  | Plus white_space -> token buf
  | Plus digit -> NUMBER (lexeme buf |> int_of_string)
  | _ -> Unknown_token (lexeme buf) |> raise
