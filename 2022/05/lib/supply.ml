open! Core

module IntMap = Map.Make(Int)

module Id =struct
  type t = char
  let make c = c
end

module Stack = struct
  type t = char Array.t

  let pop stack amount =
    let len = Array.length stack in
    if len < amount then
      None
    else
      let popped = Array.slice stack 0 amount in
      let rest = Array.slice stack amount len in
      Some (popped, rest)
end

module State = struct
  type t =  Stack.t IntMap.t

  let print_stacks stacks =
    let print_stack ~key ~data =
      let data_string =
        data
        |> Array.map ~f:(fun c -> Printf.sprintf "%c" c)
        |> String.concat_array ~sep:" "
      in
      Printf.printf "%d: [%s]\n" key data_string
    in
    IntMap.iteri stacks ~f:print_stack;;

  let from_rows lines =
    let fill index map box =
      let index = index + 1 in (* start from 1 *)
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

  let get = IntMap.find
end

type instruction =
  { amount: int
  ; from: int
  ; to_: int
  }

type program =
  { stacks: State.t
  ; instructions: instruction Array.t
  }

let get_top stack =
  try
    Some (Array.get stack 0)
  with
  | _ -> None
