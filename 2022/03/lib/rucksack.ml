open! Core

module Array = ArrayLabels

type item_type = char

type t =
  { compartment1: Char.Set.t
  ; compartment2: Char.Set.t
  }

exception OddNumberedEvents
exception NoIntersection
exception MismatchedGroups

let make items =
  let length =  items |> Array.length in
  if length % 2 = 1 then raise OddNumberedEvents;
  let half_length = length / 2 in
  let compartment1 =
    items
    |> Array.sub ~pos:0 ~len:half_length
    |> Char.Set.of_array
  in
  let compartment2 =
    items
    |> Array.sub ~pos:half_length ~len:half_length
    |> Char.Set.of_array
  in
  { compartment1; compartment2 }

let item_priority (i: item_type) =
  let open Char in
  let is_uppercase = i >= 'A' && i <= 'Z' in
  let code = i |> to_int in
  if is_uppercase
  then code - ('A' |> to_int) + 1 + 26
  else code - ('a' |> to_int) + 1

let get_first_intersetcion s =
  match Char.Set.nth s 0 with
  | None   -> NoIntersection |> raise
  | Some v -> v

let common_item_type t =
  let { compartment1; compartment2 } = t in
  let intersection = Char.Set.inter compartment1 compartment2 in
  get_first_intersetcion intersection

let to_string t =
  let { compartment1 ; compartment2 }  = t in
  let to_string comp = comp |> Char.Set.to_list |> String.of_char_list in
  Printf.sprintf "Rucksack { comp1 = %s ; comp2 = %s }" (compartment1 |> to_string) (compartment2 |> to_string)

let get_badge (a, b, c) =
  let all_items t = Char.Set.union t.compartment1 t.compartment2 in
  let common = Char.Set.inter (all_items a) (Char.Set.inter (all_items b) (all_items c)) in
  get_first_intersetcion common

let part1 values =
  values
  |> Array.map ~f:(common_item_type)
  |> Array.map ~f:(item_priority)
  |> Array.fold_left ~f:(+) ~init:0

let part2 values =
  let number_of_elves = values |> Array.length in
  if number_of_elves % 3 <> 0 then raise MismatchedGroups;
  let unfold values =
    let length = values |> Array.length in
    if length = 0 then None
    else
      let group = values |> Array.sub ~pos:0 ~len:3 in
      let rest = values |> Array.sub ~pos:3 ~len:(length-3) in
      Some ((group.(0), group.(1), group.(2)), rest)
  in
  let groups = Seq.unfold unfold values in
  groups
  |> Seq.map get_badge
  |> Seq.map item_priority
  |> Seq.fold_left (+) 0
