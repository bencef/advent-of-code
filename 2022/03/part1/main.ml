open Core
open Twentytwoday3

module Array = ArrayLabels

let () =
  Reading.from_file "input"
  |> Rucksack.part1
  |> Int.to_string
  |> print_endline
