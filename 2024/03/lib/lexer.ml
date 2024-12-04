open Sedlexing.Utf8
open Parser

exception Unknown_token of string

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

let rec op2 num1 num2 buf =
  match%sedlex buf with
  | number ->
      let num2 = Some (lexeme buf |> int_of_string) in
      op2 num1 num2 buf
  | ')' -> ( match num2 with Some num2 -> MUL (num1, num2) | None -> JUNK)
  | eof ->
      Sedlexing.rollback buf;
      JUNK
  | any -> JUNK
  | _ -> failwith "unpossible"

let rec op1 num buf =
  match%sedlex buf with
  | number -> op1 (Some (lexeme buf |> int_of_string)) buf
  | ',' -> ( match num with Some num -> op2 num None buf | None -> JUNK)
  | eof ->
      Sedlexing.rollback buf;
      JUNK
  | any -> JUNK
  | _ -> failwith "unpossible"

let mul buf =
  match%sedlex buf with
  | '(' -> op1 None buf
  | eof ->
      Sedlexing.rollback buf;
      JUNK
  | any -> JUNK
  | _ -> failwith "unpossible"

let token buf =
  match%sedlex buf with
  | eof -> EOF
  | "mul" -> mul buf
  | "do()" -> DO
  | "don't()" -> DONT
  | any -> JUNK
  | _ -> failwith "unpossible"
