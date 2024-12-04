module Signal :
  sig
    type t
    val make : char list -> t option
    val start : t -> int option
  end
