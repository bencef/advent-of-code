open! Core
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module Lookup = struct
  type dict = IntSet.t IntMap.t
  type t = { after : dict; before : dict }

  let make (pairs : (int * int) list) : t =
    let add ~key ~data m =
      let data =
        match Map.find m key with
        | None -> IntSet.singleton data
        | Some set -> Set.add set data
      in
      Map.set m ~key ~data
    in
    let merge { before; after } (x, y) =
      let before = add before ~key:y ~data:x in
      let after = add after ~key:x ~data:y in
      { before; after }
    in
    let init = { before = IntMap.empty; after = IntMap.empty } in
    List.fold pairs ~init ~f:merge

  let after { after; _ } n =
    match Map.find after n with None -> [] | Some set -> Set.to_list set

  let before { before; _ } n =
    match Map.find before n with None -> [] | Some set -> Set.to_list set
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

type t = { _lookup : Lookup.t; print_orders : Print_order.t list }

let make ordering print_orders =
  let lookup = Lookup.make ordering in
  let print_orders = List.map print_orders ~f:Print_order.make in
  { _lookup = lookup; print_orders }

let print_orders { print_orders; _ } = print_orders

module Tests = struct
  open Testing

  module Int = struct
    include Int

    let string_of_t = string_of_int
  end

  module IntList = ListOf (Int)

  let%test "one element print orders middle is itself" =
    let order = Print_order.make [ 1 ] in
    let actual = Print_order.middle order in
    let expected = 1 in
    assert_equal (module Int) actual ~expected

  let%test "three elements print orders middle is middle" =
    let order = Print_order.make [ 1; 2; 3 ] in
    let actual = Print_order.middle order in
    let expected = 2 in
    assert_equal (module Int) actual ~expected

  let%test "two after one forward" =
    let pairs = [ (1, 2) ] in
    let l = Lookup.make pairs in
    let actual = Lookup.after l 1 in
    let expected = [ 2 ] in
    assert_equal (module IntList) actual ~expected

  let%test "two after one backwards" =
    let pairs = [ (1, 2) ] in
    let l = Lookup.make pairs in
    let actual = Lookup.before l 2 in
    let expected = [ 1 ] in
    assert_equal (module IntList) actual ~expected
end
