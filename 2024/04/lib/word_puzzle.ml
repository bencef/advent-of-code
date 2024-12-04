open! Core

type letter = X | M | A | S
type t = letter array array
type index = { row : int; col : int }

let from_lists data = data |> Array.of_list |> Array.map ~f:Array.of_list

let find_indices t letter =
  Array.concat_mapi t ~f:(fun row col ->
      Array.filter_mapi col ~f:(fun col l ->
          if phys_equal l letter then Some { row; col } else None))

let star_indices _t _index = ()
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

  let%test "letter equals" = assert_equal (module Letter) X ~expected:X

  let%test "index equals" =
    assert_equal
      (module Index)
      { row = 1; col = 2 } ~expected:{ col = 2; row = 1 }

  let%test "empty puzzle has no Xs" =
    let puzzle = from_lists [] in
    let actual = find_indices puzzle X in
    assert_equal (module IndexArray) actual ~expected:[||]
end
