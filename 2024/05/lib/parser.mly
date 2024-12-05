%{
open! Core
open Printer_queue
%}

%token <int> NUMBER
%token EOF
%token SEPARATOR
%token BEFORE
%token COMMA
%start <t> parse

%%

parse:
  | orderings = nonempty_list(ordering); SEPARATOR; po = list(print_order) EOF { make orderings po }
  ;

ordering:
  | a = NUMBER; BEFORE; b = NUMBER { (a, b) }
  ;

print_order:
  | po = separated_nonempty_list(COMMA, NUMBER) { po }
  ;
