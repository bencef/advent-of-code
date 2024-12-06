open! Core
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module Lookup = struct
  type dict = IntSet.t IntMap.t
  type t = { _after : dict; _before : dict }

  let make (_pairs : (int * int) list) : t = failwith "TODO"
end

module Print_order = struct
  type t = int array

  let make (order : int list) : t =
    let res = Array.of_list order in
    if Int.(Array.length res % 2) = 0 then
      failwith "print order with even pages"
    else res

  let middle (order : t) : int = order.(Array.length order / 2)
end

type t = { _lookup : Lookup.t; _print_orders : Print_order.t list }

let make ordering print_orders =
  let lookup = Lookup.make ordering in
  let print_orders = List.map print_orders ~f:Print_order.make in
  { _lookup = lookup; _print_orders = print_orders }

module Tests = struct
  open Testing

  module Int = struct
    include Int

    let string_of_t = string_of_int
  end

  let%test "one element print orders middle is itself" =
    let order = Print_order.make [ 1 ] in
    let actual = Print_order.middle order in
    let expected = 1 in
    assert_equal (module Int) actual ~expected
end
