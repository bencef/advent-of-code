open! Core

module type S = sig
  type t

  val equal : t -> t -> bool
  val string_of_t : t -> string
end

let assert_equal (type t) (module M : S with type t = t) ~expected (actual : t)
    =
  if M.equal actual expected then true
  else (
    Printf.eprintf "\nExpected: %s\nGot:      %s\n" (M.string_of_t expected)
      (M.string_of_t actual);
    false)
