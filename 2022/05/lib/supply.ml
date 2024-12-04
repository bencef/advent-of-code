open! Core

module IntMap = Map.Make(Int)

module Id =struct
  type t = char
  let make c = c
  let to_string = String.of_char
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

  let add_rev stack ~to_add =
    let rev = Array.rev to_add in
    Array.concat [rev; stack]
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
      let stack =
        match IntMap.find map index with
        | None -> []
        | Some list -> list
      in
      let stack = match box with
      | Some box -> box :: stack
      | None -> stack
      in
      map |> IntMap.set ~key:index ~data:stack
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

  let set state key data =
    IntMap.set state ~key ~data

  let num_stacks state = IntMap.length state

  let fold = IntMap.fold
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

module Tests = struct
  let test_get state index =
    State.get state index |> Option.value_exn

  let stack_to_string stack =
    let data_string =
      stack
      |> Array.map ~f:(fun c -> Printf.sprintf "%c" c)
      |> String.concat_array ~sep:" "
    in
    Printf.sprintf "[%s]\n" data_string

  let%test "pop 0 from empty" =
    let rows = [ [ None ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 0 with
    | None -> failwith "Couldn't split empty stack"
    | Some ([||], [||]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)
end
