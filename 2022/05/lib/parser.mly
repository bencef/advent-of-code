%{
    open! Core
    open Supply
%}
%token <Int.t> NUMBER

%token EMPTY_BOX

%token MOVE
%token TO
%token FROM

%token LINE_END
%token OPEN_BRACKET CLOSE_BRACKET
%token <Supply.Id.t> ID
%token EOF
%start <program> prog
%%

prog:
  | stacks = header; LINE_END; LINE_END; instructions = instructions; EOF { { stacks; instructions } }
  ;

header:
  | bs = boxes; indices { bs }
  ;

boxes:
  | lines = nonempty_list(box_line) { State.from_rows lines }
  ;

box_line:
  | boxes = nonempty_list(possible_box); LINE_END { boxes }
  ;

possible_box:
  | EMPTY_BOX { None }
  | box = box { Some box}
  ;

box:
  | OPEN_BRACKET; box_id = ID; CLOSE_BRACKET { box_id }
  ;

indices:
  | ind = nonempty_list(padded_number) { ind }


%inline padded_number:
   | d = NUMBER { d }

instructions:
  | is = nonempty_list(instruction) { Array.of_list is }
  ;

%inline instruction:
  | MOVE; amount = NUMBER; FROM; from = NUMBER; TO; to_ = NUMBER; LINE_END { { amount; to_; from } }
