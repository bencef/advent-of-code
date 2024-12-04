open! Core

module IntMap = Map.Make(Int)

type id = char

type stack = char Array.t

type state = stack IntMap.t

type instruction =
  { amount: int
  ; from: int
  ; to_: int
  }

type program =
  { stacks: state
  ; instructions: instruction Array.t
  }

let print_stacks stacks =
  let print_stack ~key ~data =
    let data_string =
      data
      |> Array.map ~f:(fun c -> Printf.sprintf "%c" c)
      |> String.concat_array ~sep:" "
    in
    Printf.printf "%d: [%s]\n" key data_string
  in
  IntMap.iteri stacks ~f:print_stack;;
