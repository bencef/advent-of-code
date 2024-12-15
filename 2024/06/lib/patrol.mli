type facing = North | South | East | West
type sentry

val sexp_of_sentry : sentry -> Sexplib0.Sexp.t

module Tile : sig
  type t = Sentry of facing | Empty | Obstacle
end

type floor

val make_floor : Tile.t list list -> floor
val get_sentry : floor -> sentry
val count_obstacles : floor -> int
