%{
    open! Core
%}
%token EOF
%start <unit> prog
%%

prog:
  | EOF { () }
