%{
open! Core
open Program
%}

%token <int * int> MUL
%token JUNK
%token EOF
%start <t list> parse

%%

parse:
  | parts = list(part); EOF { parts }
  ;

part:
  | m = MUL { match m with | (op1, op2) -> Product (op1, op2) }
  | JUNK { Junk }
  ;
