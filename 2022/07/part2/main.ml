open Core
open Twentytwoday07
open Domain

let () =
  let fs = Reading.from_file "input" |> Option.value_exn in
  let dirs = Actions.dirs_with_size fs in
  let currently_occupied =
    match Array.to_list dirs with
    | [] -> failwith "no directories"
    | (size, "/") :: _ -> size
    | _ -> failwith "root directory is not the biggest"
  in
  let compare (size1, _) (size2, _) = Int.compare size1 size2 in
  Array.sort dirs ~compare;
  let drive_capacity = 70000000 in
  let needed_to_operate = 30000000 in
  let get_available_space size =
    let if_removed = currently_occupied - size in
    let possible_free_space = drive_capacity - if_removed in
    possible_free_space
  in
  let just_big_enough (size, _) =
    let possible_free_space = get_available_space size in
    Int.(possible_free_space >= needed_to_operate)
  in
  let to_remove = Array.find dirs ~f:just_big_enough in
  match to_remove with
  | None -> Printf.printf "No candidate directory found\n"
  | Some (size, name) ->
      let available_space = get_available_space size in
      Printf.printf
        "Dir to remove: %s with size: %d this would make the available space: %d\n"
        name size available_space
