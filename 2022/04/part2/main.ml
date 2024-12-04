open Core
open Twentytwoday4

let () =
  Reading.from_file "input"
  |> Camp.part2
  |> Int.to_string
  |> print_endline
