open! Core

type letter = X | M | A | S
type t = letter array array

let from_lists data = data |> Array.of_list |> Array.map ~f:Array.of_list
