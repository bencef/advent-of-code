open Core
open Twentytwoday3

module Array = ArrayLabels

let () =
  try
    Reading.from_file "input"
    |> Rucksack.part1
    |> Int.to_string
    |> print_endline
  with
  | Rucksack.NoIntersection t ->
     t |> Rucksack.to_string |> print_endline
