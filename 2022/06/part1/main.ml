open Core
open Twentytwoday06


let () =
  let prog = Reading.from_file "input" |> Option.value_exn in
  prog
