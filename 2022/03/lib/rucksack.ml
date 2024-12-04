open! Core

module Array = ArrayLabels

type item_type = char

type t =
  { compartment1: Char.Set.t
  ; compartment2: Char.Set.t
  }

exception OddNumberedEvents
exception NoIntersection of t

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

let common_item_type t =
  let { compartment1; compartment2 } = t in
  let intersection = Char.Set.inter compartment1 compartment2 in
  match Char.Set.nth intersection 0 with
  | Some v -> v
  | None -> raise (NoIntersection t)

let to_string t =
  let { compartment1 ; compartment2 }  = t in
  let to_string comp = comp |> Char.Set.to_list |> String.of_char_list in
  Printf.sprintf "Rucksack { comp1 = %s ; comp2 = %s }" (compartment1 |> to_string) (compartment2 |> to_string)

let part1 values =
  values
  |> Array.map ~f:(common_item_type)
  |> Array.map ~f:(item_priority)
  |> Array.fold_left ~f:(+) ~init:0
