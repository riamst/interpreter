open Core

type t

module Token : sig
  type t

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

  val equal : t -> tokentype -> bool
  val to_string : t -> string
  val sexp_of_tokentype : tokentype -> Sexp.t
end

val of_string : string -> Token.t Sequence.t
val sexp_of_token : Token.t -> Sexp.t
