%token <int> NUMBER
%token EOF
%start <(int array) * (int array)> parse

%{
open! Core

let transform nums =
  nums |> Array.of_list |> Array.unzip
%}

%%

parse:
  | nums = list(pair); EOF { transform nums }
  ;

pair:
  | a = NUMBER; b = NUMBER { (a, b) }
  ;
