open Core
open Twentytwoday2

module Array = ArrayLabels

let () =
  Reading.from_file "input"
  |> Array.map ~f:Strategy.score1
  |> Array.fold_left ~f:(+) ~init:0
  |> Int.to_string
  |> print_endline
