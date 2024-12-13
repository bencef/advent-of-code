open! Core

module Coord = struct
  type t = { row : int; col : int } [@@deriving sexp]

  let compare { row = row1; col = col1 } { row = row2; col = col2 } =
    let open Int in
    match compare row1 row2 with 0 -> compare col1 col2 | res -> res
end

module CoordSet = Set.Make (Coord)

type floor = { obstacles : CoordSet.t }

let make_floor values =
  let f obstacles (row, col) =
    let coord = { Coord.row; col } in
    Set.add obstacles coord
  in
  let obstacles = List.fold values ~init:CoordSet.empty ~f in
  { obstacles }

let count_obstacles { obstacles } = Set.length obstacles
