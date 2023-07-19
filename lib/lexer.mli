open Core

type t

module Token : sig
  type tokentype =
    | Illegal
    (* Identifiers + literals *)
    | Ident
    | Int
    | String
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
    | Lbracket
    | Rbracket
    (* Keywords *)
    | Function
    | Let
    | True
    | False
    | If
    | Else
    | Return

  type t = {
    toktype: tokentype;
    lit: string;
  }

  val sexp_of_t : t -> Sexp.t
  val equal : t -> tokentype -> bool
  val equal_op : t option -> tokentype -> bool
  val to_string : t -> string
  val sexp_of_tokentype : tokentype -> Sexp.t
end

val of_string : string -> Token.t list
