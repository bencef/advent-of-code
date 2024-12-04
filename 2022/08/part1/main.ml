open Core
open Twentytwoday08

module Trees = Domain.Trees(Log.Null_Logger)

let () =
  let prog = Reading.from_file "input" |> Option.value_exn in
  prog |> Trees.visibilities |> ignore
