{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | " "            { SPACE }
  | [ '0' - '9' ]+ { NUMBER(Lexing.lexeme lexbuf |> int_of_string) }
  | [ 'A' - 'Z' ]  { ID(Lexing.lexeme_char lexbuf 0) }
  | "\n"           { Lexing.new_line lexbuf; LINE_END }
  | "["            { OPEN_BRACKET }
  | "]"            { CLOSE_BRACKET }
  | "move"         { MOVE }
  | "from"         { FROM }
  | "to"           { TO }
  | _              { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof            { EOF }
