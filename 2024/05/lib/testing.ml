open! Core

module type S = sig
  type t

  val equal : t -> t -> bool
  val string_of_t : t -> string
end

module ListOf (M : S) : S with type t = M.t List.t = struct
  type t = M.t List.t

  let equal = List.equal M.equal
  let string_of_t = List.to_string ~f:M.string_of_t
end

module ArrayOf (M : S) : S with type t = M.t Array.t = struct
  type t = M.t Array.t

  let equal = Array.equal M.equal
  let string_of_t ts = Array.to_list ts |> List.to_string ~f:M.string_of_t
end

let assert_equal (type t) (module M : S with type t = t) ~expected (actual : t)
    =
  if M.equal actual expected then true
  else (
    Printf.eprintf "\nExpected: %s\nGot:      %s\n" (M.string_of_t expected)
      (M.string_of_t actual);
    false)
