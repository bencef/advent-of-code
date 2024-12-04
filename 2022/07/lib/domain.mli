module Fs : sig
  type file_data = { name : string; size : int }

  type dir_data = { name : string; nodes : t array }
  and t = Dir_Node of dir_data | File_Node of file_data

  val dir : string -> t
  val file : int -> string -> t

  type dir_entry = Cd_Root | Cd_Up | Cd_Dir of string
  type command_invokation = Ls of t list | Cd of dir_entry

  val from_commands : command_invokation list -> t
end

module Actions : sig
  val dirs_with_size : Fs.t -> (int * string) array
end
