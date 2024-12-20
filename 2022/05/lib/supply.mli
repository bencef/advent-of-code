module Id : sig
  type t
  val make : char -> t
  val to_string : t -> string
end

module Stack : sig
  type t
  val pop : t -> int -> (t * t) option
  val add_rev : t -> to_add:t -> t
  val get_top : t -> Id.t option
end

module State : sig
  type t
  val from_rows : Id.t option list list -> t
  val print_stacks : t -> unit
  val num_stacks : t -> int
  val fold : t -> init:'a -> f:(key:int -> data:Stack.t -> 'a -> 'a) -> 'a
  val get : t -> int -> Stack.t option
  val set : t -> int -> Stack.t -> t
end

type instruction = { amount : int; from : int; to_ : int; }

module Program : sig
  type t = { stacks : State.t; instructions : instruction array; }
  val run_9000 : t -> State.t
  val run_9001 : t -> State.t
end
