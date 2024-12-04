module Trees : functor (Logger : Log.S) -> sig
  type t
  type dir = Top | Left | Right | Bottom

  val make : int list list -> t
  val visibilities : t -> bool array array
  val seeing_distance : t -> int array array
end

val to_string : bool array array -> string
