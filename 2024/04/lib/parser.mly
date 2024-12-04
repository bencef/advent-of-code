%{
open Word_puzzle
%}

%token EOF
%token NEWLINE
%token X
%token M
%token A
%token S
%start <t> parse

%%

parse:
  | rows = list(row); EOF { from_lists rows }
  ;

row:
  | letters = list(letter); NEWLINE { letters }
  ;

%inline letter:
  | X { X }
  | M { M }
  | A { A }
  | S { S }
  ;
