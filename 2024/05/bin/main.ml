open Core
open Aoc_2024_05

let inputs = ref []
let spec = []
let anon_fun input = inputs := input :: !inputs

let usage =
  "PROG [input_file]\t-\tReads input from file or if not given then from \
   standard input"

let read_from_stdin () = Reading.from_channel In_channel.stdin

let read_from_file file_name =
  In_channel.with_file file_name ~f:Reading.from_channel

let read_lists () =
  Arg.parse spec anon_fun usage;
  match !inputs with
  | [] -> read_from_stdin ()
  | file_name :: [] -> read_from_file file_name
  | _ -> failwith usage

let greet num = Printf.printf "Hello, part %d\n" num
