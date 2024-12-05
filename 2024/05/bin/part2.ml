open! Core
module IntMap = Map.Make (Int)

let get_frequencies nums =
  let counting_merge m num =
    let count = match Map.find m num with None -> 0 | Some n -> n in
    Map.set m ~key:num ~data:(count + 1)
  in
  ArrayLabels.fold_left ~f:counting_merge ~init:IntMap.empty nums

let () =
  let a, b = Main.read_lists () in
  let b = get_frequencies b in
  let get_score n =
    let right_value = Option.value ~default:0 (Map.find b n) in
    n * right_value
  in
  let result = Array.fold a ~init:0 ~f:(fun acc n -> acc + (get_score n)) in
  Printf.printf "%d\n" result
