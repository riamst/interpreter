open Core
open Lexer

type token = Token.t
type tokenseq = token Sequence.t [@@deriving sexp_of]
type 'a pResult = (tokenseq * 'a, string) Result.t [@@deriving sexp_of]
type 'a parser = tokenseq -> 'a pResult

type expr =
  | Let of (expr * expr)
  | Return of expr
  | Int_lit of int
  | Ident of string
[@@deriving sexp]

let map ~(f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun input ->
  match p input with
  | Ok (input', x) -> Ok (input', f x)
  | Error error -> Error error
;;

let tag (token_tag : Token.tokentype) : string parser =
 fun input ->
  match Sequence.next input with
  | Some (t, rest) when Token.equal t token_tag -> Ok (rest, Token.to_string t)
  | Some (t, _) ->
    Error
      (Printf.sprintf "expected %s, found %s\n"
         (token_tag |> Token.sexp_of_tokentype |> Sexp.to_string_hum)
         (t |> Lexer.sexp_of_token |> Sexp.to_string_hum)
      )
  | None -> Error "End of File"
;;

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
 fun input ->
  p1 input |> Result.map ~f:(fun (input', _) -> p2 input') |> Result.join
;;

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
 fun input ->
  input
  |> p1
  |> Result.map ~f:(fun (input', x) ->
         input' |> p2 |> Result.map ~f:(fun (input'', _) -> (input'', x))
     )
  |> Result.join
;;

let ( <*> ) (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
 fun input ->
  input
  |> p1
  |> Result.map ~f:(fun (input', x) ->
         input' |> p2 |> Result.map ~f:(fun (input'', y) -> (input'', (x, y)))
     )
  |> Result.join
;;

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun (input : tokenseq) ->
  match p1 input with
  | Ok (input', x) -> Ok (input', x)
  | Error left_error ->
    input
    |> p2
    |> Result.map_error ~f:(fun right_error ->
           Printf.sprintf "%s or %s" left_error right_error
       )
;;

let optional (p : 'a parser) : 'a option parser =
 fun input ->
  match p input with
  | Ok (input', x) -> Ok (input', Some x)
  | Error _ -> Ok (input, None)
;;

let many (p : 'a parser) : 'a list parser =
 fun input ->
  let result = ref [] in
  let rec loop input =
    match p input with
    | Ok (rest, x) ->
      result := x :: !result;
      loop rest
    | Error _ -> input
  in
  let rest = loop input in
  Ok (rest, List.rev !result)
;;

let int = tag Token.Int |> map ~f:(fun a -> Int_lit (Int.of_string a))
let expression = int
let ident = tag Token.Ident |> map ~f:(fun a -> Ident a)

let let_s : expr parser =
  let p1 =
    tag Token.Let *> ident
    <* tag Token.Assign
    <*> expression
    <* optional (tag Token.Semicolon)
  in
  map ~f:(fun (a, b) -> Let (a, b)) p1
;;

let return_s : expr parser =
  let p1 = tag Token.Return *> expression <* optional (tag Token.Semicolon) in
  map ~f:(fun a -> Return a) p1
;;

let statement = let_s <|> return_s
