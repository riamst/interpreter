open Core
open Lexer

type 'a pResult = (Token.t list * 'a, string) Result.t [@@deriving sexp_of]
type 'a parser = Token.t list -> 'a pResult

type expr =
  | Let of (expr * expr)
  | Return of expr
  | Int_lit of int
  | Bool_lit of bool
  | Ident of string
  | Prefix of prefix_op
  | Infix of infix_op

and infix_op =
  | Add of (expr * expr)
  | Sub of (expr * expr)
  | Mul of (expr * expr)
  | Div of (expr * expr)
  | Equ of (expr * expr)
  | Neq of (expr * expr)
  | Les of (expr * expr)
  | Gre of (expr * expr)

and prefix_op =
  | Not of expr
  | Neg of expr
[@@deriving sexp]

let map ~(f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun input ->
  match p input with
  | Ok (input', x) -> Ok (input', f x)
  | Error e -> Error e
;;

let tag (token_tag : Token.tokentype) : string parser =
 fun input ->
  match input with
  | t :: rest when Token.equal t token_tag -> Ok (rest, Token.to_string t)
  | t :: _ ->
    Error
      (Printf.sprintf "expected %s, found %s\n"
         (token_tag |> Token.sexp_of_tokentype |> Sexp.to_string_hum)
         (t |> Token.sexp_of_t |> Sexp.to_string_hum)
      )
  | [] -> Error "End of File"
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
 fun input ->
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
let ident = tag Token.Ident |> map ~f:(fun a -> Ident a)
let btrue = tag Token.True |> map ~f:(fun _ -> Bool_lit true)
let bfalse = tag Token.True |> map ~f:(fun _ -> Bool_lit false)

(* OPERATOR PRECEDENCE: *)
let lowest = 0
let equals = 1
let lesgre = 2
let addsub = 3
let muldiv = 4
let prefix = 5
let fncall = 6

let prec tok =
  let open Token in
  match tok.toktype with
  | Plus | Minus -> addsub
  | Asterisk | Slash -> muldiv
  | Lt | Gt -> lesgre
  | Equals | Not_equals -> equals
  | _ -> lowest
;;

let rec expression ~(minp : int) input : expr pResult =
  let curr_token = List.hd input in
  let parsed_exp =
    ref
      ( match
          curr_token
          |> Option.bind ~f:(fun a -> get_prefix_fn a)
          |> Option.map ~f:(fun pre_fn -> pre_fn input)
        with
      | Some a -> a
      | None -> Error "bigfail"
      )
  in
  match !parsed_exp with
  | Error e -> Error e
  | Ok (rest, exp) ->
    let exp = ref exp in
    let rest = ref rest in
    while
      match List.hd !rest with
      | Some curr_token ->
        (not (Token.equal curr_token Token.Semicolon)) && minp < prec curr_token
      | None -> false
    do
      match !rest with
      | t :: _ -> (
        (parsed_exp :=
           match
             get_infix_fn t |> Option.map ~f:(fun pre_fn -> pre_fn !exp !rest)
           with
           | Some a -> a
           | None -> Error "Could not find infix function"
        );
        match !parsed_exp with
        | Ok (res, ex) ->
          rest := res;
          exp := ex
        | Error e -> failwith ("Error parsing infix " ^ e)
      )
      | [] -> failwith "unreachable"
    done;
    !parsed_exp

and minus_pre input =
  (tag Token.Minus *> expression ~minp:prefix
  |> map ~f:(fun a -> Prefix (Neg a))
  )
    input

and bang_pre input =
  (tag Token.Bang *> expression ~minp:prefix |> map ~f:(fun a -> Prefix (Not a)))
    input

and add lhs input =
  (tag Token.Plus *> expression ~minp:addsub
  |> map ~f:(fun a -> Infix (Add (lhs, a)))
  )
    input

and sub lhs input =
  (tag Token.Minus *> expression ~minp:addsub
  |> map ~f:(fun a -> Infix (Sub (lhs, a)))
  )
    input

and mul lhs input =
  (tag Token.Asterisk *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Mul (lhs, a)))
  )
    input

and div lhs input =
  (tag Token.Slash *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Div (lhs, a)))
  )
    input

and equ lhs input =
  (tag Token.Equals *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Equ (lhs, a)))
  )
    input

and neq lhs input =
  (tag Token.Not_equals *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Neq (lhs, a)))
  )
    input

and les lhs input =
  (tag Token.Lt *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Les (lhs, a)))
  )
    input

and gre lhs input =
  (tag Token.Gt *> expression ~minp:muldiv
  |> map ~f:(fun a -> Infix (Gre (lhs, a)))
  )
    input

and grouped input =
  (tag Token.Lparen *> expression ~minp:lowest <* tag Token.Rparen) input

and get_prefix_fn (tok : Token.t) : expr parser option =
  let open Token in
  match tok.toktype with
  | Minus -> Some minus_pre
  | Bang -> Some bang_pre
  | Ident -> Some ident
  | Int -> Some int
  | True -> Some btrue
  | False -> Some bfalse
  | Lparen -> Some grouped
  | _ -> None

and get_infix_fn (tok : Token.t) : (expr -> expr parser) option =
  let open Token in
  match tok.toktype with
  | Plus -> Some add
  | Minus -> Some sub
  | Asterisk -> Some mul
  | Slash -> Some div
  | Equals -> Some equ
  | Not_equals -> Some neq
  | Lt -> Some les
  | Gt -> Some gre
  | _ -> None
;;

let let_s : expr parser =
  tag Token.Let *> ident
  <* tag Token.Assign
  <*> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun (a, b) -> Let (a, b))
;;

let return_s : expr parser =
  tag Token.Return *> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Return a)
;;

let expression_s : expr parser =
  expression ~minp:lowest <* optional (tag Token.Semicolon)
;;

let statement = let_s <|> return_s <|> expression_s
let parse ~using:(p : 'a parser) str = str |> Lexer.of_string |> p
