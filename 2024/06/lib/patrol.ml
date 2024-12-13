open! Core

module Coord = struct
  type t = { row : int; col : int } [@@deriving sexp]

  let compare { row = row1; col = col1 } { row = row2; col = col2 } =
    let open Int in
    match compare row1 row2 with 0 -> compare col1 col2 | res -> res
end

module CoordSet = Set.Make (Coord)

type facing = North | South | East | West [@@deriving sexp]
type sentry = { facing : facing; coord : Coord.t } [@@deriving sexp]

type 'a floor_abs = {
  obstacles : CoordSet.t;
  width : int;
  height : int;
  sentry : 'a;
}

type floor = sentry floor_abs

module Tile = struct
  type t = Sentry of facing | Empty | Obstacle
end

let add_sentry (floor : sentry option floor_abs) facing =
  match floor.sentry with
  | None ->
      {
        floor with
        sentry =
          Some { facing; coord = { col = floor.width; row = floor.height } };
      }
  | Some _ -> failwith "Multiple sentries"

let add_obstacle (floor : sentry option floor_abs) =
  let { obstacles; _ } = floor in
  let coord = { Coord.row = floor.height; col = floor.width } in
  let obstacles = Set.add obstacles coord in
  { floor with obstacles }

let make_floor values =
  let init : sentry option floor_abs =
    let obstacles = CoordSet.empty in
    { obstacles; width = 0; height = 0; sentry = None }
  in
  let f acc row =
    let f acc tile =
      let acc =
        match tile with
        | Tile.Sentry facing -> add_sentry acc facing
        | Empty -> acc
        | Obstacle -> add_obstacle acc
      in
      { acc with width = acc.width + 1 }
    in
    let acc = List.fold row ~init:acc ~f in
    { acc with height = acc.height + 1 }
  in
  let built = List.fold values ~init ~f in
  match built.sentry with
  | None -> failwith "No sentry on the map"
  | Some sentry -> { built with sentry }

let count_obstacles { obstacles; _ } = Set.length obstacles
