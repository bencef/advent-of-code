%{
    open! Core
    open Supply
%}
%token EOF
%start <program> prog
%%

prog:
  | stacks = header; instructions = instructions; EOF { { stacks; instructions } }
  ;

header:
  | (* TODO *) { IntMap.empty }
  ;

instructions:
  | (* TODO *) { [||] }
  ;
