open Core
open Twentytwoday5

let run_prog prog =
  let open Supply in
  let {stacks; instructions} = prog in
  let step_one stacks {amount; from; to_} =
    let maybe_update =
      let (let*) =  Option.Let_syntax.(>>=) in
      let* to_stack = State.get stacks to_ in
      let* from_stack = State.get stacks from in
      let* (to_add, from_stack) = Stack.pop from_stack amount in
      let stacks = State.set stacks from from_stack in
      let to_stack = Stack.add_rev ~to_add to_stack in
      let stacks = State.set stacks to_ to_stack in
      Some stacks
    in maybe_update |> Option.value ~default:stacks
  in
  Array.fold instructions ~init:stacks ~f:step_one

let get_tops (stacks: Supply.State.t) =
  let open Supply in
  let default = Id.make '_' in
  let append_top ~key ~data acc =
    Array.unsafe_set acc (key-1) (Supply.get_top data |> Option.value ~default);
    acc
  in
  let init = Array.init (State.num_stacks stacks) ~f:(fun _ -> default) in
  let tops =
    State.fold stacks ~init ~f:append_top
    |> Array.map ~f:Id.to_string
  in
  String.concat_array tops

let () =
  let prog = Reading.from_file "input" |> Option.value_exn in
  let stacks = run_prog prog in
  let tops = get_tops stacks in
  Printf.printf "%s\n" tops
