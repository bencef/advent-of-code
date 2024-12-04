{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | ' '                           { read lexbuf }
  | "/"                           { ROOT }
  | ".."                          { UP_DIR }
  | "cd"                          { CD }
  | "ls"                          { LS }
  | "dir"                         { DIR }
  | "$"                           { PROMPT }
  | [ 'a' - 'z' 'A' - 'Z' '.' ] + { NAME(lexeme lexbuf) }
  | [ '0' - '9' ] +               { SIZE(lexeme lexbuf |> int_of_string) }
  | '\n'                          { new_line lexbuf; NEW_LINE }
  | _                             { failwith (Printf.sprintf "Unknown token: %s" (Lexing.lexeme lexbuf)) }
  | eof                           { EOF }
