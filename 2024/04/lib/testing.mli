module type S = sig
  type t

  val equal : t -> t -> bool
  val string_of_t : t -> string
end

module ListOf : functor (M : S) -> S with type t = M.t list
module ArrayOf : functor (M : S) -> S with type t = M.t array

val assert_equal : (module S with type t = 't) -> expected:'t -> 't -> bool
