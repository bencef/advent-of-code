open Core

module type S = sig
  val log : ('a, Out_channel.t, unit) format -> 'a
end

module Null_Logger : S = struct
  let log fmt = Printf.ifprintf Stdio.stdout fmt
end

module Console_Logger : S = struct
  let log = Printf.printf
end
