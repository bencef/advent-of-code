open! Core

let get_dampening_indices max_index =
  let amount = Int.min (max_index + 1) 3 in
  Seq.init amount (fun n -> max_index - n)

let remove_at ~index record =
  List.filteri ~f:(fun idx _ -> Int.(index = idx |> not)) record

let dampen record index =
  let indices = get_dampening_indices index in
  Seq.map (fun idx -> remove_at ~index:idx record) indices

let is_safe_with_dampening record =
  match Main.is_record_safe record with Main.Safe -> true | _ -> false

let is_record_safe record =
  match Main.is_record_safe record with
  | Main.Safe -> true
  | Failed_at index ->
      let modified_records = dampen record index in
      Seq.exists is_safe_with_dampening modified_records

let () =
  let records = Main.read_lists () in
  let result = List.count records ~f:is_record_safe in
  Printf.printf "%d\n" result
