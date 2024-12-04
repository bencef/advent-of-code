%{
    open! Core
    open Domain.Signal
%}
%token EOF
%token <char> CHAR
%start <t option> prog
%%

prog:
  | chars = CHAR+; EOF { chars |> make }
