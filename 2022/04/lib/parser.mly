%{
  open! Core
%}
%token <Camp.id> ID
%token DASH COMMA
%token EOF
%start <(Camp.elf * Camp.elf) Array.t> prog
%%

prog:
  | pairs = list(pair); EOF { pairs |> Array.of_list }
  ;

pair:
  | r1 = range; COMMA; r2 = range { (r1, r2) }
  ;

range:
  | id1 = ID; DASH; id2 = ID { let sections = Camp.Range.make id1 id2 in
                               { Camp.sections }}
