open Core
open Twentytwoday07

let () =
  let prog = Reading.from_file "input" |> Option.value_exn in
  prog |> ignore
