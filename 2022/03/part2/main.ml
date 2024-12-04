open Core
open Twentytwoday3

module Array = ArrayLabels

let () =
  Reading.from_file "input"
  |> Rucksack.part2
  |> Int.to_string
  |> print_endline
