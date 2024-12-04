%{
  open Core
%}
%token ROCK PAPER SCISSORS
%token EOF
%start <Strategy.t Option.t> prog
%%

prog:
| EOF { None }
