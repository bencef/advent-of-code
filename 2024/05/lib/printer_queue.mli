module Lookup : sig
  type t
end

module Print_order : sig
  type t

  val middle : t -> int
end

type t

val make : (int * int) list -> int list list -> t
