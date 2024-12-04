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

  let add stack ~to_add =
    Array.concat [to_add; stack]

  let get_top stack =
    try
      Some (Array.get stack 0)
    with
    | _ -> None

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

module Program = struct
  type t = { stacks: State.t
           ; instructions: instruction Array.t
           }

  let run prog ~append =
    let {stacks; instructions} = prog in
    let step_one stacks {amount; from; to_} =
      let maybe_update =
        let (let*) =  Option.Let_syntax.(>>=) in
        let* to_stack = State.get stacks to_ in
        let* from_stack = State.get stacks from in
        let* (to_add, from_stack) = Stack.pop from_stack amount in
        let stacks = State.set stacks from from_stack in
        let to_stack = append to_stack ~to_add in
        let stacks = State.set stacks to_ to_stack in
        Some stacks
      in maybe_update |> Option.value ~default:stacks
    in
    Array.fold instructions ~init:stacks ~f:step_one

    let run_9000 prog = run prog ~append:Stack.add_rev
    let run_9001 prog = run prog ~append:Stack.add

end

module Tests = struct
  let test_get state index =
    State.get state index |> Option.value_exn

  let stack_to_string stack =
    let data_string =
      stack
      |> Array.map ~f:(fun c -> Printf.sprintf "%c" c)
      |> String.concat_array ~sep:"; "
    in
    Printf.sprintf "[|%s|]" data_string

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

  let%test "pop 1 from empty" =
    let rows = [ [ None ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 1 with
    | None -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 1 from singleton" =
    let rows = [ [ Some 'K' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 1 with
    | None -> false
    | Some ([|'K'|], [||]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 1 from two" =
    let rows = [ [ Some 'A' ]
               ; [ Some 'B' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 1 with
    | None -> false
    | Some ([|'A'|], [|'B'|]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 1 from three" =
    let rows = [ [ Some 'A' ]
               ; [ Some 'B' ]
               ; [ Some 'C' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 1 with
    | None -> false
    | Some ([|'A'|], [|'B'; 'C'|]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 2 from three" =
    let rows = [ [ Some 'A' ]
               ; [ Some 'B' ]
               ; [ Some 'C' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 2 with
    | None -> false
    | Some ([|'A'; 'B'|], [|'C'|]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 2 from two" =
    let rows = [ [ Some 'A' ]
               ; [ Some 'B' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 2 with
    | None -> false
    | Some ([|'A'; 'B'|], [||]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "pop 3 from three" =
    let rows = [ [ Some 'A' ]
               ; [ Some 'B' ]
               ; [ Some 'C' ] ] in
    let state = State.from_rows rows in
    let stack = test_get state 1 in
    match Stack.pop stack 3 with
    | None -> false
    | Some ([|'A'; 'B'; 'C'|], [||]) -> true
    | Some (a, b) ->
       let a = stack_to_string a in
       let b = stack_to_string b in
       failwith (Printf.sprintf "Unexpected result: (%s, %s)" a b)

  let%test "move zero to zero" =
    let rows = [ [ None; None ] ] in
    let state = State.from_rows rows in
    let source_stack = test_get state 1 in
    let target_stack = test_get state 2 in
    match Stack.add_rev ~to_add:source_stack target_stack with
    | [||] -> true
    | a ->
       let a = stack_to_string a in
       failwith (Printf.sprintf "Unexpected result: %s" a)

  let%test "move three to three" =
    let rows = [ [ Some 'A'; Some 'D' ]
               ; [ Some 'B'; Some 'E' ]
               ; [ Some 'C'; Some 'F' ] ] in
    let state = State.from_rows rows in
    let source_stack = test_get state 1 in
    let target_stack = test_get state 2 in
    match Stack.add_rev ~to_add:source_stack target_stack with
    | [|'C'; 'B'; 'A'; 'D'; 'E'; 'F'|] -> true
    | a ->
       let a = stack_to_string a in
       failwith (Printf.sprintf "Unexpected result: %s" a)
end
