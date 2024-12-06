module Print_order : sig
  type t

  val middle : t -> int
end

type t

val make : (int * int) list -> int list list -> t
val print_orders : t -> Print_order.t list
val is_in_order : t -> Print_order.t -> bool
