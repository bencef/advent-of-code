open Core
open Twentytwoday1

module Array = ArrayLabels

let () =
  let elves = Reading.from_file "input" in
  let number_of_elves = elves |> Array.length in
  let calories = elves |> Array.map ~f:(Elf.get_all_calories) in
  calories |> Array.sort ~cmp:(Int.compare);
  let top_three = Array.sub calories ~pos:(number_of_elves-3) ~len:3 in
  let sum = top_three |> Array.fold_left ~f:(+) ~init:0 in
  print_endline (sum |> Int.to_string)
