%{
open! Core
open Patrol
%}

%token <int> NUMBER
%token EOF
%start <floor> parse

%%

parse:
  | nums = list(pair); EOF { make_floor nums }
  ;

pair:
  | a = NUMBER; b = NUMBER { (a, b) }
  ;
