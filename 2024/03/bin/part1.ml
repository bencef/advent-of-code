open! Core

let print_program_endline = function
  | Aoc_2024_03.Program.Product (a, b) ->
      Printf.printf "%5d * %5d = %d\n" a b (a * b)
  | _ -> print_endline "JUNK"

let () =
  let programs = Main.read_lists () in
  List.iter programs ~f:print_program_endline
