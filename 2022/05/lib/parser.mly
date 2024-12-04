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
%token BOX_LINE_END
%token OPEN_BRACKET CLOSE_BRACKET
%token <Supply.id> ID
%token EMPTY_BOX
%token EOF
%start <program> prog
%%

prog:
  | stacks = header; instructions = instructions; EOF { { stacks; instructions } }
  ;

header:
  | bs = boxes; indices { bs }
  ;

boxes:
  | lines = nonempty_list(box_line) { transpose lines }
  ;

box_line:
  | boxes = nonempty_list(possible_box); BOX_LINE_END { boxes }
  ;

possible_box:
  | EMPTY_BOX { None }
  | box = box { Some box}
  ;

box:
  | OPEN_BRACKET; box_id = ID; CLOSE_BRACKET { box_id }
  ;

indices:
  | (* TODO *) { () }
  ;

instructions:
  | (* TODO *) { [||] }
  ;
