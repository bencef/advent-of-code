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

  let%test "letter equals" = assert_equal (module Letter) X ~expected:X
end
