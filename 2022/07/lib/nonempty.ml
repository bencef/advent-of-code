open! Core

type 'a t = 'a * 'a list

let from = function [] -> None | head :: rest -> Some (head, rest)
let singleton x = (x, [])
let deconstruct = Fun.id
let push v (head, rest) = (v, head :: rest)
