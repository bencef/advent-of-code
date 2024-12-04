open Core
open Twentytwoday08
module Trees = Domain.Trees (Log.Null_Logger)

let count visibilities =
  let counts =
    Array.map visibilities ~f:(fun row -> Array.count row ~f:Fun.id)
  in
  Array.reduce_exn counts ~f:Int.( + )

let () =
  let forest = Reading.from_file "input" |> Option.value_exn in
  let visibilities = forest |> Trees.visibilities in
  let count = count visibilities in
  let forest = Domain.to_string visibilities in
  Printf.printf "The forest:\n---\n%s\n" forest;
  Printf.printf "Number of visible trees are: %d\n" count
