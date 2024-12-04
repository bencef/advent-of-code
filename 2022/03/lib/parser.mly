%{
  open! Core
%}
%token <char> ITEM
%token EOF
%token SEPARATOR
%start <Rucksack.t Array.t> prog
%%

prog:
  | separated_list(SEPARATOR, rucksack); EOF { $1 |> Array.of_list }
  ;

rucksack:
  | list(ITEM) { $1 |> Array.of_list |> Rucksack.make }
  ;
