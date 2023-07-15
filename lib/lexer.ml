open Core

module Token = struct
  type tokentype =
    | Illegal
    (* Identifiers + literals *)
    | Ident
    | Int
    (* Operators *)
    | Assign
    | Plus
    | Minus
    | Bang
    | Asterisk
    | Slash
    | Lt
    | Gt
    | Equals
    | Not_equals
    (* Delimiters *)
    | Comma
    | Semicolon
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    (* Keywords *)
    | Function
    | Let
    | True
    | False
    | If
    | Else
    | Return
  [@@deriving sexp]

  type t = {
    toktype: tokentype;
    lit: string;
  }
  [@@deriving sexp]

  let is_ident_char a = Char.is_alphanum a || Char.equal '_' a

  let ident_or_keyword = function
    | "fn" -> { toktype = Function; lit = "fn" }
    | "let" -> { toktype = Let; lit = "let" }
    | "true" -> { toktype = True; lit = "true" }
    | "if" -> { toktype = If; lit = "if" }
    | "else" -> { toktype = Else; lit = "else" }
    | "return" -> { toktype = Return; lit = "return" }
    | a -> { toktype = Ident; lit = a }
  ;;

  let int a = { toktype = Int; lit = a }
end

type t = {
  input: string;
  pos: int;
  read_pos: int;
  ch: char option;
}
[@@deriving sexp]

let build str = { input = str; pos = 0; read_pos = 0; ch = None }

let read_char { input; read_pos; _ } =
  {
    input;
    pos = read_pos;
    read_pos = read_pos + 1;
    ch = (try Some (String.get input read_pos) with Invalid_argument _ -> None);
  }
;;

let peek_char { input; read_pos; _ } =
  try Some (String.get input read_pos) with Invalid_argument _ -> None
;;

let expect_peek lex a = Char.equal (Option.value ~default:'0' (peek_char lex)) a

let read_while ~f lex =
  let str = String.subo ?pos:(Some lex.pos) lex.input in
  let str = String.take_while ~f str in
  let pos = lex.pos + String.length str in
  let next_lex =
    {
      lex with
      pos;
      read_pos = pos + 1;
      ch =
        (try Some (String.get lex.input pos) with Invalid_argument _ -> None);
    }
  in
  (str, next_lex)
;;

let skip_ws lex : t =
  let lexe = ref lex in
  while Char.is_whitespace (Option.value ~default:'0' !lexe.ch) do
    lexe := read_char !lexe
  done;
  !lexe
;;

let of_char a lex =
  let open Token in
  let tok =
    match a with
    | '=' when expect_peek lex '=' ->
      let lex = read_char lex in
      { toktype = Equals; lit = "==" }
    | '=' -> { toktype = Assign; lit = String.of_char '=' }
    | ';' -> { toktype = Semicolon; lit = String.of_char ';' }
    | '(' -> { toktype = Lparen; lit = String.of_char '(' }
    | ')' -> { toktype = Lparen; lit = String.of_char ')' }
    | ',' -> { toktype = Comma; lit = String.of_char ',' }
    | '+' -> { toktype = Plus; lit = String.of_char '+' }
    | '{' -> { toktype = Lbrace; lit = String.of_char '{' }
    | '}' -> { toktype = Rbrace; lit = String.of_char '}' }
    | '-' -> { toktype = Minus; lit = String.of_char '-' }
    | '!' when expect_peek lex '=' ->
      let lex = read_char lex in
      { toktype = Not_equals; lit = "!=" }
    | '!' -> { toktype = Bang; lit = String.of_char '!' }
    | '*' -> { toktype = Asterisk; lit = String.of_char '*' }
    | '/' -> { toktype = Slash; lit = String.of_char '/' }
    | '<' -> { toktype = Lt; lit = String.of_char '<' }
    | '>' -> { toktype = Gt; lit = String.of_char '>' }
    | a -> { toktype = Illegal; lit = String.of_char a }
  in
  (tok, read_char lex)
;;

let next_token lex =
  let lex = skip_ws lex in
  let open Token in
  match lex.ch with
  | Some a when Char.is_alpha a ->
    let str, next_lex = read_while ~f:is_ident_char lex in
    let tok = ident_or_keyword str in
    Some (tok, next_lex)
  | Some a when Char.is_digit a ->
    let str, next_lex = read_while ~f:Char.is_digit lex in
    let tok = int str in
    Some (tok, next_lex)
  | Some a -> Some (of_char a lex)
  | None -> None
;;

let to_seq lex = Sequence.unfold ~init:lex ~f:next_token
let of_string str = str |> build |> read_char |> to_seq
let sexp_of_token = Token.sexp_of_t
