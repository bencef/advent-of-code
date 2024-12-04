type letter = X | M | A | S
type t
type index

val from_lists : letter list list -> t
val find_indices : t -> letter -> index array
val star_indices : t -> index -> unit
val at : t -> index -> letter
