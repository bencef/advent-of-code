open! Core

let () =
  let a, b = Main.read_lists () in
  let compare = Int.compare in
  Array.sort ~compare a;
  Array.sort ~compare b;
  let res = ref 0 in
  for i = 0 to Array.length a - 1 do
    let distance = Int.abs (b.(i) - a.(i)) in
    res := distance + !res
  done;
  Printf.printf "%d\n" !res
