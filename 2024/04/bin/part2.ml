open! Core
open Aoc_2024_04.Word_puzzle

let pattern = [| M; A; S |]

let check_coords puzzle coord_list =
  let rec go idx =
    if idx = Array.length pattern then true
    else
      let in_puzzle = at puzzle coord_list.(idx) in
      if phys_equal pattern.(idx) in_puzzle then go (idx + 1) else false
  in
  go 0

let check_pos puzzle coord_lists =
  Array.count coord_lists ~f:(check_coords puzzle) > 1

let () =
  let puzzle = Main.read_puzzle () in
  let x_indices = find_indices puzzle A in
  let runs_to_check = Array.map x_indices ~f:(cross_indices puzzle) in
  let result =
    Array.count runs_to_check ~f:(check_pos puzzle)
  in
  Printf.printf "%d\n" result
