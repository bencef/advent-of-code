open Core
open Twentytwoday06
open Domain.Signal

let () =
  let signal =
    Reading.from_file "input"
    |> Option.value_exn ~message:"Parse error"
    |> Option.value_exn ~message:"Input not long enough"
  in
  let start_pos =
    start signal |> Option.value_exn ~message:"No start position found"
  in
  Printf.printf "Signal starts at position %d\n" start_pos
