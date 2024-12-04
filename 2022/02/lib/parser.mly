%{
  open Core
%}
%token ROCK PAPER SCISSORS
%token X Y Z
%token SEPARATOR
%token EOF
%start <Strategy.step Array.t> prog
%%

prog:
  | list(step); EOF { $1 |> Array.of_list }
  ;

step:
  | m = move; SEPARATOR; s = strategy { (m, s) }

move:
  | ROCK     { Strategy.Rock }
  | PAPER    { Strategy.Paper }
  | SCISSORS { Strategy.Scissors }
  ;

strategy:
  | X { Strategy.X }
  | Y { Strategy.Y }
  | Z { Strategy.Z }
  ;
