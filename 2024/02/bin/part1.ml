open! Core

let is_record_safe record =
  match Main.is_record_safe record with Main.Safe -> true | _ -> false

let () =
  let records = Main.read_lists () in
  let result = List.count records ~f:is_record_safe in
  Printf.printf "%d\n" result
