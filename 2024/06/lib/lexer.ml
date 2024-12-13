open Sedlexing.Utf8
open Parser
open Patrol

exception Unknown_token of string

let rec token buf =
  match%sedlex buf with
  | '.' -> EMPTY
  | '#' -> OBSTACLE
  | '>' -> SENTRY East
  | 'v' -> SENTRY South
  | '<' -> SENTRY West
  | '^' -> SENTRY North
  | eof -> EOF
  | '\n' ->
      Sedlexing.new_line buf;
      EOL
  | Plus white_space -> token buf
  | _ -> Unknown_token (lexeme buf) |> raise
