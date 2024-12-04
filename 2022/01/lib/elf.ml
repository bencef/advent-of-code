open Core

type calories = int

type t =
  { food: calories Array.t;
  }

let make food = { food }

let get_all_calories t =
  t.food |> ArrayLabels.fold_left ~f:(+) ~init:0
