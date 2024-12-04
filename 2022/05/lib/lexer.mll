{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | " "            { after_space lexbuf }
  | [ '0' - '9' ]+ { NUMBER(Lexing.lexeme lexbuf |> int_of_string) }
  | [ 'A' - 'Z' ]  { ID(Lexing.lexeme_char lexbuf 0) }
  | "\n"           { Lexing.new_line lexbuf; LINE_END }
  | "["            { OPEN_BRACKET }
  | "]"            { CLOSE_BRACKET }
  | "move"         { MOVE }
  | _              { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof            { EOF }
and after_space =
  parse
  | "   " { EMPTY_BOX }
  | [ '0' - '9' ]+ { NUMBER(Lexing.lexeme lexbuf |> int_of_string) }
  (* indices are padded *)
  | "  " [ '0' - '9' ]+ { NUMBER(Lexing.lexeme lexbuf |> String.chop_prefix_exn ~prefix:"  " |> int_of_string) }
  | "["            { OPEN_BRACKET }
  | "from"         { FROM }
  | "to"           { TO }
  | "\n"           { Lexing.new_line lexbuf; LINE_END }
  | _              { failwith (Printf.sprintf "Unknown token after space: \"%s\"" (Lexing.lexeme lexbuf)) }
