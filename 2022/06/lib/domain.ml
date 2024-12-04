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

  let is_at_start_marker { window; _ } =
    let set = CharSet.of_array window in
    Int.(CharSet.length set = 4)

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

  let start signal =
    let rec start' signal index =
      if signal |> is_at_start_marker then Some (index, signal)
      else
        match step signal with
        | None -> None
        | Some signal -> start' signal (index + 1)
    in
    let start_index = 4 in
    start' signal start_index

  let message_start _signal = None
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
end
