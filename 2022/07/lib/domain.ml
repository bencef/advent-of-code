open! Core

module Fs = struct
  type file_data = { name : string; size : int }

  type dir_data = { name : string; nodes : t array }
  and t = Dir_Node of dir_data | File_Node of file_data

  type entry = Dir of string | File of file_data
  type dir_entry = Cd_Root | Cd_Up | Cd_Dir of string
  type command_invokation = Ls of entry list | Cd of dir_entry

  let initial_fs = Dir_Node { name = "/"; nodes = [||] }

  module Zipper = struct
    type crumb = { name : string; siblings : t array }
    type z = { trail : crumb list; focus : t option }

    let from_fs = function
      | Dir_Node { name; nodes = _ } ->
          let trail = [ { name; siblings = [||] } ] in
          { trail; focus = None }
      | File_Node _ -> failwith "Can't make a zipper out of a file"

    let rec to_fs { trail; focus } =
      match trail with
      | [] -> failwith "no more crumbs"
      | { name; siblings } :: trail -> (
          let d =
            let focus = Option.to_array focus in
            let nodes = Array.append siblings focus in
            Dir_Node { name; nodes }
          in
          match trail with [] -> d | _ -> to_fs { trail; focus = Some d })
  end

  let from_commands _commmands = initial_fs

  let rec equal fs_1 fs_2 =
    match (fs_1, fs_2) with
    | File_Node data1, File_Node data2 ->
        String.(equal data1.name data2.name)
        && Int.(equal data1.size data2.size)
    | Dir_Node data1, Dir_Node data2 ->
        String.(equal data1.name data2.name)
        && Array.equal equal data1.nodes data2.nodes
    | _ -> false

  let to_string = function
    | File_Node data -> Printf.sprintf "File: %d %s" data.size data.name
    | Dir_Node data ->
        Printf.sprintf "Dir: %s files: %d" data.name (data.nodes |> Array.length)
end

module Tests = struct
  open Fs

  let assert_equal actual ~expected =
    if equal actual expected then true
    else (
      Printf.printf "Expected: %s\nGot: %s\n" (to_string expected)
        (to_string actual);
      false)

  let%test "initial FS can be zipped back up" =
    let z = Zipper.from_fs initial_fs in
    let fs = Zipper.to_fs z in
    assert_equal fs ~expected:initial_fs
end
