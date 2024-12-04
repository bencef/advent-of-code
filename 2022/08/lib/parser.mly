%parameter <Trees: sig type t val make : int list list -> t end>
%{
    open! Core
%}

%start <Trees.t> prog
%%

prog:
  | rows = row+; EOF { Trees.make rows }

row:
  | ds = DIGIT+; LINE_END { ds }
