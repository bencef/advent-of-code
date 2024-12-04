open Core
open Twentytwoday4

let () =
  Reading.from_file "input"
  |> Camp.part1
  |> Int.to_string
  |> print_endline
