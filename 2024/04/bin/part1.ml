open! Core
open Aoc_2024_04.Word_puzzle

let pattern = [| X; M; A; S |]

let check_coords puzzle coord_list =
  let rec go idx =
    if idx = Array.length pattern then true
    else
      let in_puzzle = at puzzle coord_list.(idx) in
      if phys_equal pattern.(idx) in_puzzle then go (idx + 1) else false
  in
  go 0

let () =
  let puzzle = Main.read_puzzle () in
  let x_indices = find_indices puzzle X in
  let coords_to_check = Array.concat_map x_indices ~f:(star_indices puzzle) in
  let result =
    Array.filter coords_to_check ~f:(check_coords puzzle) |> Array.length
  in
  Printf.printf "%d\n" result
