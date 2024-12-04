%{
    open! Core
    open Supply

    let transpose lines =
      let fill index map box =
        match box with
        | Some box ->
           let stack =
             match IntMap.find map index with
             | None -> []
             | Some list -> list
           in
           let stack = box :: stack in
           map |> IntMap.set ~key:index ~data:stack
        | None -> map
      in
      let add_line map line =
        line
        |> List.foldi ~f:fill ~init:map
      in
      lines
      |> List.fold ~f:(add_line) ~init:IntMap.empty
      |> IntMap.map ~f:List.rev  (* TODO: do we need this? *)
      |> IntMap.map ~f:Array.of_list

%}
%token SPACE
%token <Int.t> NUMBER

%token MOVE
%token TO
%token FROM

%token LINE_END
%token OPEN_BRACKET CLOSE_BRACKET
%token <Supply.id> ID
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
  | lines = nonempty_list(box_line) { transpose lines }
  ;

box_line:
  | boxes = separated_nonempty_list(SPACE, possible_box); LINE_END { boxes }
  ;

possible_box:
  | SPACE; SPACE; SPACE { None }
  | box = box { Some box}
  ;

box:
  | OPEN_BRACKET; box_id = ID; CLOSE_BRACKET { box_id }
  ;

indices:
  | ind = separated_nonempty_list(SPACE, padded_number) { ind }


%inline padded_number:
   | SPACE; d = NUMBER; SPACE { d }

instructions:
  | is = nonempty_list(instruction) { Array.of_list is }
  ;

%inline instruction:
  | MOVE; SPACE; amount = NUMBER; SPACE; FROM; SPACE; from = NUMBER; SPACE; TO; SPACE; to_ = NUMBER; LINE_END { { amount; to_; from } }
