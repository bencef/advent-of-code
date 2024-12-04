%{
  open! Core
%}
%token EOF
%start <Supply.program Option.t> prog
%%

prog:
  | EOF { None }
  ;
