open! Core
open Aoc_2024_05.Printer_queue

let () =
  let pq = Main.read_lists () in
  let orders = print_orders pq in
  let res =
    List.filter orders ~f:(is_in_order pq)
    |> List.map ~f:Print_order.middle
    |> List.reduce ~f:Int.( + )
  in
  match res with
  | None -> failwith "Not enough orders to sum"
  | Some res -> Printf.printf "%d\n" res
