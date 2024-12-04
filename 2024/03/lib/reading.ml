open Sedlexing.Utf8

let with_lexbuf lexbuf =
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.parse in
  parser lexer

let from_string s =
  let lexbuf = from_string s in
  with_lexbuf lexbuf

let from_channel ch =
  let lexbuf = from_channel ch in
  with_lexbuf lexbuf
