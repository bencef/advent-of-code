open! Core
open Aoc_2024_03

let collect = ref true

let collect_product = function
  | Program.Product (a, b) -> if !collect then Some (a * b) else None
  | Do ->
      collect := true;
      None
  | Dont ->
      collect := false;
      None
  | _ -> None

let () =
  let programs = Main.read_lists () in
  let result =
    List.filter_map programs ~f:collect_product
    |> List.reduce ~f:Int.( + ) |> Option.value ~default:0
  in
  Printf.printf "%d\n" result
