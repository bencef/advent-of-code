open Core
open Twentytwoday08
module Trees = Domain.Trees (Log.Null_Logger)

let max trees =
  let find_max arr = Array.reduce_exn arr ~f:Int.max in
  let row_maxes = Array.map trees ~f:find_max in
  find_max row_maxes

let () =
  let forest = Reading.from_file "input" |> Option.value_exn in
  let seeing_distances = forest |> Trees.seeing_distance in
  let forest =
    Domain.to_string ~string_of_tree:string_of_int seeing_distances
  in
  Printf.printf "The forest:\n---\n%s\n" forest;
  Printf.printf "The greatest visibility score is: %d\n" (max seeing_distances)
