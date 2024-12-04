open! Core

module IntMap = Map.Make(Int)

type id = char

type stack = char Array.t

type state = stack IntMap.t

type instruction =
  { from: int
  ; to_: int
  }

type program =
  { stacks: state
  ; instructions: instruction Array.t
  }
