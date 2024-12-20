module Signal : sig
  type t

  val make : char list -> t option
  val start : t -> (int * t) option
  val message_start : t -> (int*t) option
end
