%{
open! Core
open Program
%}

%token <int * int> MUL
%token DO
%token DONT
%token JUNK
%token EOF
%start <t list> parse

%%

parse:
  | parts = list(part); EOF { parts }
  ;

part:
  | m = MUL { match m with | (op1, op2) -> Product (op1, op2) }
  | DO { Do }
  | DONT { Dont }
  | JUNK { Junk }
  ;
