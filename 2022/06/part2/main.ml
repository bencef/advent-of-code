open Core
open Twentytwoday06
open Domain.Signal

let () =
  let signal =
    Reading.from_file "input"
    |> Option.value_exn ~message:"Parse error"
    |> Option.value_exn ~message:"Input not long enough"
  in
  let message_start, _ =
    message_start signal
    |> Option.value_exn ~message:"No message position found"
  in
  Printf.printf "First message starts at position %d\n" message_start
