open Core
open Twentytwoday08

let () =
  let prog = Reading.from_file "input" |> Option.value_exn in
  prog
