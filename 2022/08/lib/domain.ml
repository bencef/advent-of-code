open Core

module Trees (Logger : Log.S) = struct
  type t = { rows : int array array }
  type dir = [ `Top ]

  let make rows =
    Logger.log "Reading %d rows\n" (List.length rows);
    let rows = Array.of_list_map rows ~f:Array.of_list in
    { rows }

  let visibilities { rows } =
    Array.map rows ~f:(fun row -> Array.map row ~f:(fun _ -> true))
end

module Tests = struct
  module Trees_No_Logs = Trees (Log.Null_Logger)
  module Trees_Logs = Trees (Log.Console_Logger)

  let to_string a =
    let string_of_bool = function true -> " " | false -> "X" in
    let row_to_string row =
      let row =
        Array.map row ~f:string_of_bool |> String.concat_array ~sep:" "
      in
      Printf.sprintf "|%s|" row
    in
    let rows = Array.map a ~f:row_to_string |> String.concat_array ~sep:"\n" in
    Printf.sprintf "\n%s\n" rows

  let assert_equals got ~expected =
    let row_equal a b = Array.equal Bool.equal a b in
    match Array.equal row_equal got expected with
    | false ->
        let got = to_string got in
        let expected = to_string expected in
        Printf.printf "Got: %s\nExpected: %s\n" got expected;
        false
    | true -> true

  let%test "empty forest" =
    let module Trees = Trees_No_Logs in
    let rows = [] in
    let forest = Trees.make rows in
    let visibilities = [||] in
    Array.length visibilities = Array.length (Trees.visibilities forest)

  let%test "single tree is always visible" =
    let module Trees = Trees_No_Logs in
    let rows = [ [ 0 ] ] in
    let forest = Trees.make rows in
    let visibilities = Trees.visibilities forest in
    let expected = [| [| true |] |] in
    assert_equals visibilities ~expected

  let%test "outer layers are always visible" =
    let module Trees = Trees_No_Logs in
    let rows = [ [ 0; 9 ]; [ 9; 0 ] ] in
    let expected = [| [| true; true |]; [| true; true |] |] in
    let forest = Trees.make rows in
    let visibilities = Trees.visibilities forest in
    assert_equals visibilities ~expected

  let%test "smae height is not visible" =
    let module Trees = Trees_No_Logs in
    let rows = [ [ 0; 0; 0 ]; [ 0; 0; 0 ]; [ 0; 0; 0 ] ] in
    let expected =
      [|
        [| true; true; true |]; [| true; false; true |]; [| true; true; true |];
      |]
    in
    let forest = Trees.make rows in
    let visibilities = Trees.visibilities forest in
    assert_equals visibilities ~expected

  let%test "example 1" =
    let module Trees = Trees_No_Logs in
    let rows =
      [
        [ 3; 0; 3; 7; 3 ];
        [ 2; 5; 5; 1; 2 ];
        [ 6; 5; 3; 3; 2 ];
        [ 3; 3; 5; 4; 9 ];
        [ 3; 5; 3; 9; 0 ];
      ]
    in
    let expected =
      [|
        [| true; true; true; true; true |];
        [| true; true; true; false; true |];
        [| true; true; false; true; true |];
        [| true; false; true; false; true |];
        [| true; true; true; true; true |];
      |]
    in
    let forest = Trees.make rows in
    let visibilities = Trees.visibilities forest in
    assert_equals visibilities ~expected
end
