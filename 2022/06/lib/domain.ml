open! Core

module Signal = struct

type window = char array
type t = { window : window; rest : char list }

let make = function
  | a :: b :: c :: d :: rest ->
      let window = [| a; b; c; d |] in
      Some { window; rest }
  | _ -> None
end
