%token <int> NUMBER
%token EOF
%token EOR
%start <int list list> parse

%{
open! Core
%}

%%

parse:
  | recs = list(record); EOF { recs }
  ;

record:
  | levels = list(NUMBER); EOR { levels }
  ;
