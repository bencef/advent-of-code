open Core

module type S = sig
  val log : ((('a, Out_channel.t, unit) format -> 'a) -> unit) -> unit
end

module Null_Logger : S = struct
  let log = ignore
end

module Console_Logger : S = struct
  let log k = k Printf.printf
end
