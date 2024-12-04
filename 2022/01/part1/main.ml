open Core
open Twentytwoday1

let () =
  Reading.from_file "input"
  |> ArrayLabels.map ~f:Elf.get_all_calories
  |> ArrayLabels.fold_left ~f:Int.max ~init:Int.min_value
  |> Int.to_string
  |> print_endline
