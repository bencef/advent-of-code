type letter = X | M | A | S
type t
type index

val from_lists : letter list list -> t
val find_indices : t -> letter -> index array
val star_indices : t -> index -> index array array
val cross_indices : t -> index -> index array array
val at : t -> index -> letter
