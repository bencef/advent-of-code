open Core
open Twentytwoday5

let get_tops (stacks: Supply.State.t) =
  let open Supply in
  let default = Id.make '_' in
  let append_top ~key ~data acc =
    Array.unsafe_set acc (key-1) (Stack.get_top data |> Option.value ~default);
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
  let stacks = Supply.Program.run_9000 prog in
  let tops = get_tops stacks in
  Printf.printf "%s\n" tops
