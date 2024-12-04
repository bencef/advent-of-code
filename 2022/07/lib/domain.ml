open! Core

module Fs = struct
  type file_data = { name : string; size : int }

  type dir_data = { name : string; nodes : t array }
  and t = Dir_Node of dir_data | File_Node of file_data

  let dir name = Dir_Node { name; nodes = [||] }
  let file size name = File_Node { size; name }

  type dir_entry = Cd_Root | Cd_Up | Cd_Dir of string
  type command_invokation = Ls of t list | Cd of dir_entry

  let initial_fs = Dir_Node { name = "/"; nodes = [||] }

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

  let get_name = function
    | Dir_Node { name; _ } -> name
    | File_Node { name; _ } -> name

  module Zipper = struct
    type crumb = { name : string; siblings : t array }
    type z = { trail : crumb Nonempty.t }

    let from_fs = function
      | Dir_Node { name; nodes = _ } ->
          let trail = Nonempty.singleton { name; siblings = [||] } in
          { trail }
      | File_Node _ -> failwith "Can't make a zipper out of a file"

    let up z =
      match Nonempty.deconstruct z.trail with
      | _, [] -> failwith "can't go up from root"
      | { name; siblings = nodes }, crumb :: trail ->
          let dir = Dir_Node { name; nodes } in
          let siblings = Array.append crumb.siblings [| dir |] in
          let crumb = { crumb with siblings } in
          let trail =
            match Nonempty.from trail with
            | None -> Nonempty.singleton crumb
            | Some trail -> Nonempty.push crumb trail
          in
          { trail }

    let rec top z =
      match Nonempty.deconstruct z.trail with _, [] -> z | _ -> top (up z)

    let to_fs z =
      let { trail } = top z in
      let { name; siblings }, _empty_trail = Nonempty.deconstruct trail in
      Dir_Node { name; nodes = siblings }

    let commit { trail } entry =
      let { name; siblings }, trail = Nonempty.deconstruct trail in
      let siblings = Array.append siblings [| entry |] in
      let crumb = { name; siblings } in
      let trail =
        match Nonempty.from trail with
        | None -> Nonempty.singleton crumb
        | Some trail -> Nonempty.push crumb trail
      in
      { trail }

    let add_entry z entry =
      Printf.printf "Adding: %s\n" (to_string entry);
      commit z entry

    let add_entries z entries = List.fold entries ~init:z ~f:add_entry
  end

  let run_command z = function
    | Ls entries -> Zipper.add_entries z entries
    | Cd Cd_Root -> Zipper.top z
    | Cd Cd_Up -> Zipper.up z
    | Cd (Cd_Dir name) ->
        Printf.printf "FIXME: $ cd %s\n" name;
        z

  let from_commands commands =
    let z = Zipper.from_fs initial_fs in
    List.fold commands ~init:z ~f:run_command |> Zipper.to_fs
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

  let%test "initial FS with one file can be zipped back up" =
    let z = Zipper.from_fs initial_fs in
    let file = File_Node { name = "passwd"; size = 42 } in
    let z = Zipper.add_entry z file in
    let fs = Zipper.to_fs z in
    let expected = Dir_Node { name = get_name fs; nodes = [| file |] } in
    assert_equal fs ~expected

  let%test "initial FS with two files can be zipped back up" =
    let z = Zipper.from_fs initial_fs in
    let file_1 = File_Node { name = "passwd"; size = 42 } in
    let file_2 = File_Node { name = "groups"; size = 142 } in
    let z = Zipper.add_entry z file_1 in
    let z = Zipper.add_entry z file_2 in
    let fs = Zipper.to_fs z in
    let expected =
      Dir_Node { name = get_name fs; nodes = [| file_1; file_2 |] }
    in
    assert_equal fs ~expected
end
