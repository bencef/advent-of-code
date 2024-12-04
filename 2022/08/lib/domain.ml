open Core

module Trees (Logger : Log.S) = struct
  type t = { rows : int array array }
  type dir = Top | Left | Right | Bottom

  let make rows =
    Logger.log "Reading %d rows\n" (List.length rows);
    let rows = Array.of_list_map rows ~f:Array.of_list in
    { rows }

  let look_down dir trees =
    let process_tree (highest, acc) tree =
      match highest with
      | Some highest when highest >= tree -> (Some highest, [] :: acc)
      | _ -> (Some tree, [ dir ] :: acc)
    in
    let _highest, visibilities =
      Array.fold trees ~init:(None, []) ~f:process_tree
    in
    Array.of_list_rev visibilities

  let merge a b =
    (* I need to learn optics *)
    Array.map2_exn a b ~f:(fun a b -> Array.map2_exn a b ~f:List.append)

  let dir_visibilities { rows } =
    let lefts = Array.map rows ~f:(look_down Left) in
    let rights =
      let collect row = look_down Right (Array.rev row) |> Array.rev in
      Array.map rows ~f:collect
    in
    let transposed = Array.transpose_exn rows in
    let tops = Array.map transposed ~f:(look_down Top) |> Array.transpose_exn in
    let bottoms =
      let collect row = look_down Bottom (Array.rev row) |> Array.rev in
      Array.map transposed ~f:collect |> Array.transpose_exn
    in
    List.reduce [ lefts; rights; tops; bottoms ] ~f:merge
    |> Option.value ~default:[||]

  let visibilities forest =
    let dir_visibilities = dir_visibilities forest in
    Array.map dir_visibilities ~f:(fun row ->
        Array.map row ~f:(fun dirs -> List.length dirs > 0))
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
