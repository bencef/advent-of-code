open! Core

type letter = X | M | A | S
type t = letter array array
type index = { row : int; col : int }

let from_lists data = data |> Array.of_list |> Array.map ~f:Array.of_list

let find_indices t letter =
  Array.concat_mapi t ~f:(fun row col ->
      Array.filter_mapi col ~f:(fun col l ->
          if phys_equal l letter then Some { row; col } else None))

let star_indices t index =
  let row_num = Array.length t in
  let col_num = if row_num = 0 then 0 else Array.length t.(0) in
  let inside_puzzle { row; col } =
    let valid actual ~max_val = Int.(actual >= 0 && actual < max_val) in
    valid row ~max_val:row_num && valid col ~max_val:col_num
  in
  if not (inside_puzzle index) then [||]
  else
    let steps =
      let modify row_f col_f { row; col } =
        { row = row_f row; col = col_f col }
      in
      let inc x = x + 1 in
      let dec x = x - 1 in
      let stay x = x in
      let dirs = [ stay; inc; dec ] in
      (* this generates a stay/stay, but that should be fine, just drop the head *)
      List.concat_map dirs ~f:(fun row_dir ->
          List.map dirs ~f:(fun col_dir -> modify row_dir col_dir))
      |> fun l -> List.drop l 1
    in
    List.filter_map steps ~f:(fun step ->
        let open Sequence in
        let res =
          unfold ~f:(fun last -> Some (last, step last)) ~init:index
          |> Fun.flip take 4 |> filter ~f:inside_puzzle |> to_array
        in
        if Array.length res = 4 then Some res else None)
    |> Array.of_list

let at t { row; col } = t.(row).(col)

module Tests = struct
  open Testing

  module Letter = struct
    type t = letter

    let equal = phys_equal
    let string_of_t = function X -> "X" | M -> "M" | A -> "A" | S -> "S"
  end

  module Index = struct
    type t = index

    let equal { row = row1; col = col1 } { row = row2; col = col2 } =
      Int.(row1 = row2 && col1 = col2)

    let string_of_t { row; col } = Printf.sprintf "{Row: %3d\tCol: %3d}" row col
  end

  module IndexArray = ArrayOf (Index)
  module IndexArrayArray = ArrayOf (IndexArray)

  let%test "letter equals" = assert_equal (module Letter) X ~expected:X

  let%test "index equals" =
    assert_equal
      (module Index)
      { row = 1; col = 2 } ~expected:{ col = 2; row = 1 }

  let%test "empty puzzle has no Xs" =
    let puzzle = from_lists [] in
    let actual = find_indices puzzle X in
    assert_equal (module IndexArray) actual ~expected:[||]

  let%test "two lines puzzle has two Xs" =
    let puzzle = from_lists [ [ S; S; S; X ]; [ S; X; S; S ] ] in
    let actual = find_indices puzzle X in
    let expected = [| { row = 0; col = 3 }; { row = 1; col = 1 } |] in
    assert_equal (module IndexArray) actual ~expected

  let%test "empty puzzle has no star formation at 0,0" =
    let puzzle = from_lists [] in
    let actual = star_indices puzzle { row = 0; col = 0 } in
    assert_equal (module IndexArrayArray) actual ~expected:[||]

  let%test "single xmas has one star formation" =
    let puzzle = from_lists [ [ X; X; X; X ] ] in
    let actual = star_indices puzzle { row = 0; col = 0 } in
    let expected =
      [|
        [|
          { row = 0; col = 0 };
          { row = 0; col = 1 };
          { row = 0; col = 2 };
          { row = 0; col = 3 };
        |];
      |]
    in
    assert_equal (module IndexArrayArray) actual ~expected

  let%test "two lines have one star formation" =
    let puzzle = from_lists [ [ X; X; X; X ]; [ X; X; X; X ] ] in
    let actual = star_indices puzzle { row = 0; col = 0 } in
    let expected =
      [|
        [|
          { row = 0; col = 0 };
          { row = 0; col = 1 };
          { row = 0; col = 2 };
          { row = 0; col = 3 };
        |];
      |]
    in
    assert_equal (module IndexArrayArray) actual ~expected

  let%test "four lines have 3 star formation" =
    let puzzle =
      from_lists
        [ [ X; X; X; X ]; [ X; X; X; X ]; [ X; X; X; X ]; [ X; X; X; X ] ]
    in
    let actual = star_indices puzzle { row = 0; col = 0 } in
    let expected =
      [|
        (* stay inc *)
        [|
          { row = 0; col = 0 };
          { row = 0; col = 1 };
          { row = 0; col = 2 };
          { row = 0; col = 3 };
        |];
        (* inc stay *)
        [|
          { row = 0; col = 0 };
          { row = 1; col = 0 };
          { row = 2; col = 0 };
          { row = 3; col = 0 };
        |];
        (* inc inc *)
        [|
          { row = 0; col = 0 };
          { row = 1; col = 1 };
          { row = 2; col = 2 };
          { row = 3; col = 3 };
        |];
      |]
    in
    assert_equal (module IndexArrayArray) actual ~expected

  let%test "single xmas has one star formation from the back" =
    let puzzle = from_lists [ [ X; X; X; X ] ] in
    let actual = star_indices puzzle { row = 0; col = 3 } in
    let expected =
      [|
        [|
          { row = 0; col = 3 };
          { row = 0; col = 2 };
          { row = 0; col = 1 };
          { row = 0; col = 0 };
        |];
      |]
    in
    assert_equal (module IndexArrayArray) actual ~expected
end
