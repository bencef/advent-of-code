{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | 'A' { ROCK }
  | 'B' { PAPER }
  | 'C' { SCISSORS }
  | eof { EOF }
