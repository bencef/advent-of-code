{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | 'A'  { ROCK }
  | 'B'  { PAPER }
  | 'C'  { SCISSORS }
  | 'X'  { X }
  | 'Y'  { Y }
  | 'Z'  { Z }
  | ' '  { SEPARATOR }
  | '\n' { new_line lexbuf; read lexbuf }
  | eof  { EOF }
