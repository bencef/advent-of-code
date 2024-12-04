{
  open! Core
  open! Lexing
  open Parser
}

rule read =
  parse
  | eof { EOF }
