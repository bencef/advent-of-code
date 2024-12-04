open Core

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let from_file file_name =
  In_channel.with_file file_name ~f:(fun in_c ->
      let lexbuf = Lexing.from_channel in_c in
      try
        Some (Parser.prog Lexer.read lexbuf)
      with
      | Parser.Error -> fprintf stdout "%a: syntax error\n" print_position lexbuf; None
    )
