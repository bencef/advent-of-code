val read_lists : unit -> int list list
val greet : int -> Base.unit

type step_dir = Asc | Desc | Stagnate
type check_res = Safe | Failed_at of int

val is_record_safe : int list -> check_res
