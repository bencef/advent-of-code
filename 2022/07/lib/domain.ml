module Fs = struct
  type t = unit
  type entry = Dir of string | File of { name : string; size : int }
  type dir_entry = | Cd_Root | Cd_Up | Cd_Dir of string
  type command_invokation =
    | Ls of entry list
    | Cd of dir_entry

  let from_commands _commmands = ()
end
