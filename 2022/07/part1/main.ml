open Core
open Twentytwoday07
open Domain

let () =
  let fs = Reading.from_file "input" |> Option.value_exn in
  let dirs = Actions.dirs_with_size fs in
  let dirs =
    Array.filter_map dirs ~f:(fun (size, _name) ->
        if size <= 100000 then Some size else None)
  in
  let result = Array.reduce dirs ~f:Int.( + ) |> Option.value ~default:0 in
  Printf.printf "Sum of sufficiently small directories: %d\n" result
