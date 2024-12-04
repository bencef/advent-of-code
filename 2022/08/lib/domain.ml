open Core

module Trees(Logger: Log.S) = struct
  type t = { rows : int array array }
  type dir = [ `Top]

  let make rows =
    Logger.log "Reading %d rows\n" (List.length rows);
    let rows = Array.of_list_map rows ~f:Array.of_list in
    { rows }

  let visibilities { rows } = rows
end
