open! Core

module Array = ArrayLabels

type item_type = char

type t =
  { compartment1: Char.Set.t
  ; compartment2: Char.Set.t
  }

exception OddNumberedEvents

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
