%{
  open! Core
%}
%token <string> ITEM
%token EOF
%start <Rucksack.t Array.t> prog
%%

prog:
  | rs = list(rucksack); EOF { rs |> Array.of_list }
  ;

rucksack:
  | items = ITEM { items
                   |> String.fold ~f:(fun xs x -> x :: xs) ~init:[]
                   |> Array.of_list
                   |> Rucksack.make }
  ;
