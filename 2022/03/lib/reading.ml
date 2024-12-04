open Core

let from_file file_name =
  In_channel.with_file file_name ~f:(fun in_c ->
      let lexbuf = Lexing.from_channel in_c in
      Parser.prog Lexer.read lexbuf
    )
