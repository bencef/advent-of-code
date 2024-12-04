{
  open Core
  open Lexing
  open Parser
}

rule read =
  parse
  | '\n' +          { let len = lexeme lexbuf |> String.length in
                      for _ = 1 to len do
                        new_line lexbuf
                      done;
                      if len = 1 then read lexbuf else SEPARATOR
                    }
  | [ '0' - '9' ] + { CALORIES (lexeme lexbuf |> Int.of_string)}
  | eof             { EOF }
