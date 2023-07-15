open Core

type t

module Token : sig
  type t
end

val of_string : string -> Token.t Sequence.t
val sexp_of_token : Token.t -> Sexp.t
