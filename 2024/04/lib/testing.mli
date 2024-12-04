module type S = sig
  type t

  val equal : t -> t -> bool
  val string_of_t : t -> string
end

val assert_equal : (module S with type t = 't) -> expected:'t -> 't -> bool
