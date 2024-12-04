open! Core

module Signal = struct
  type window = char array
  type t = { window : window; rest : char list }

  let make = function
    | a :: b :: c :: d :: rest ->
        let window = [| a; b; c; d |] in
        Some { window; rest }
    | _ -> None

  let is_at_start_marker { window; _ } =
    let module CharSet = Set.Make (Char) in
    let set = CharSet.of_array window in
    Int.(CharSet.length set = 4)

  let step { window; rest } =
    match (window, rest) with
    | [| _; a; b; c |], d :: rest -> Some { window = [| a; b; c; d |]; rest }
    | _, _ -> None

  let start signal =
    let rec start' signal index =
      if signal |> is_at_start_marker then Some index
      else
        match step signal with
        | None -> None
        | Some signal -> start' signal (index + 1)
    in
    let start_index = 4 in
    start' signal start_index
end

module Tests = struct
  let assert_starter_index ~signal ~expected_index =
    let open Signal in
    let signal = String.to_list signal |> make |> Option.value_exn in
    let signal_start = start signal in
    match signal_start with
    | None -> failwith "Didn't find a signal starter\n"
    | Some start when Int.(start = expected_index) -> true
    | Some start ->
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
