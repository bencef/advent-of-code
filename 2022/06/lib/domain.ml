open! Core
module CharSet = Set.Make (Char)

module Signal = struct
  type window = char array
  type t = { window : window; rest : char list }

  let make_with_window_size size chars =
    let rec make to_consume chars accum =
      if Int.(to_consume = 0) then
        let window = accum |> Array.of_list_rev in
        Some { window; rest = chars }
      else
        match chars with
        | [] -> None
        | curr :: chars -> make (to_consume - 1) chars (curr :: accum)
    in
    let initial_window = [] in
    make size chars initial_window

  let make = make_with_window_size 4

  let window_has_distinct_chars ~amount window =
    let set = CharSet.of_array window in
    Int.(CharSet.length set = amount)

  let is_at_start_marker { window; _ } =
    window_has_distinct_chars ~amount:4 window

  let is_at_message_marker { window; _ } =
    window_has_distinct_chars ~amount:14 window

  let step { window; rest } =
    match rest with
    | [] -> None
    | next :: rest ->
        let window =
          let len = Array.length window in
          let start = Array.slice window 1 len in
          Array.append start [| next |]
        in
        Some { window; rest }

  let rec seek ~is_found signal index =
    if signal |> is_found then Some (index, signal)
    else
      match step signal with
      | None -> None
      | Some signal -> seek ~is_found signal (index + 1)

  let start signal =
    let start_index = Array.length signal.window in
    seek ~is_found:is_at_start_marker signal start_index

  let message_start signal =
    let ( let* ) = Option.Let_syntax.( >>= ) in
    let message_marker_len = 14 in
    (* WOW: messages don't start after packet marker *)
    (* let* index, signal = start signal in *)
    let index = 4 in
    let* signal = make_with_window_size message_marker_len signal.rest in
    let index = index + message_marker_len in
    seek ~is_found:is_at_message_marker signal index
end

module Tests = struct
  let assert_starter_index ~signal ~expected_index =
    let open Signal in
    let signal = String.to_list signal |> make |> Option.value_exn in
    let signal_start = start signal in
    match signal_start with
    | None -> failwith "Didn't find a signal starter\n"
    | Some (start, _) when Int.(start = expected_index) -> true
    | Some (start, _) ->
        failwith
          (Printf.sprintf
             "Found signal starter at: %d, expected signal starter at: %d\n"
             start expected_index)

  let assert_message_index ~signal ~expected_index =
    let open Signal in
    let signal = String.to_list signal |> make |> Option.value_exn in
    let message_start = message_start signal in
    match message_start with
    | None -> failwith "Didn't find a signal starter\n"
    | Some (start, _) when Int.(start = expected_index) -> true
    | Some (start, _) ->
        failwith
          (Printf.sprintf
             "Found signal starter at: %d, expected signal starter at: %d\n"
             start expected_index)

  let%test "exmple in text" =
    let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb" in
    assert_starter_index ~signal:input ~expected_index:7

  let%test "given example 1" =
    let input = "bvwbjplbgvbhsrlpgdmjqwftvncz" in
    assert_starter_index ~signal:input ~expected_index:5

  let%test "given example 2" =
    let input = "nppdvjthqldpwncqszvftbrmjlhg" in
    assert_starter_index ~signal:input ~expected_index:6

  let%test "given example 3" =
    let input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" in
    assert_starter_index ~signal:input ~expected_index:10

  let%test "given example 4" =
    let input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" in
    assert_starter_index ~signal:input ~expected_index:11

                         (* Part 2 *)

  let%test "part 2 given example 1" =
    let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb" in
    assert_message_index ~signal:input ~expected_index:19

  let%test "part 2 given example 2" =
    let input = "bvwbjplbgvbhsrlpgdmjqwftvncz" in
    assert_message_index ~signal:input ~expected_index:23

  let%test "part 2 given example 3" =
    let input = "nppdvjthqldpwncqszvftbrmjlhg" in
    assert_message_index ~signal:input ~expected_index:23

  let%test "part 2 given example 4" =
    let input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" in
    assert_message_index ~signal:input ~expected_index:29

  let%test "part 2 given example 5" =
    let input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" in
    assert_message_index ~signal:input ~expected_index:26

end
