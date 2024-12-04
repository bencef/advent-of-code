open! Core

type id = int

module Range = struct
  type 'a t =
    { start: 'a
    ; end_: 'a
    }

  let make a b =
    let open Int in
    let start = min a b in
    let end_ = max a b in
    { start; end_ }

  let get_overlap a b =
    let open Int in
    let start = max a.start b.start in
    let end_ = min a.end_ b.end_ in
    if start <= end_
    then Some { start; end_ }
    else None

  let (=) a b =
    let open Int in
    a.start = b.start && a.end_ = b.end_

  let is_full_overlap a b =
    match get_overlap a b with
    | Some overlap ->
       a = overlap || b = overlap
    | None -> false
end


type elf = { sections: id Range.t }

let part1 pairs =
  let has_overlap (a, b) = Range.is_full_overlap a.sections b.sections in
  pairs
  |> Array.map ~f:(has_overlap)
  |> Array.count ~f:(Fun.id)

let part2 pairs =
  let has_overlap (a, b) =
    Range.get_overlap a.sections b.sections
    |> Option.is_some
  in
  pairs
  |> Array.map ~f:(has_overlap)
  |> Array.count ~f:(Fun.id)
