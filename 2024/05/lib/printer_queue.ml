type t = { _ordering : (int * int) list; _print_orders : int list list }

let make ordering print_orders =
  { _ordering = ordering; _print_orders = print_orders }
