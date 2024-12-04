type 'a t
val from : 'a list -> 'a t option
val singleton : 'a -> 'a t
val deconstruct : 'a t -> 'a * 'a list
val push : 'a -> 'a t -> 'a t
