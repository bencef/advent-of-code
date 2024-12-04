open Core
open Aoc_2024_02

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

type step_dir = Asc | Desc | Stagnate
type check_res = Safe | Failed_at of int

let get_step a b =
  let step_dir = if a < b then Asc else if a > b then Desc else Stagnate in
  (step_dir, Int.abs (a - b))

let same_dir orig new_dir =
  match Option.map orig ~f:(fun d -> phys_equal d new_dir) with
  | Some false -> false
  | _ -> true

let check a b rest =
  let rec go a b rest step_dir index =
    match get_step a b with
    | Stagnate, _ -> Failed_at index
    | dir, (1 | 2 | 3) ->
        if same_dir step_dir dir then
          match rest with
          | next :: rest -> go b next rest (Some dir) (index + 1)
          | _ -> Safe
        else Failed_at index
    | _ -> Failed_at index
  in
  go a b rest None 0

let is_record_safe = function
  | first_level :: second_level :: rest -> check first_level second_level rest
  | _ -> failwith "record has not enough levels"
