open Core
open Lexer
open Parsik

type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Equ
  | Neq
  | Les
  | Gre
  | Not
  | Neg
[@@deriving sexp_of]

and block = expr list

and expr =
  | Let of expr * expr
  | Return of expr
  | Int_lit of int
  | Bool_lit of bool
  | Array_lit of expr list
  | Ident of string
  | String_lit of string
  | Hash_lit of (expr * expr) list
  | Index of expr * expr
  | Un_op of operator * expr
  | Bin_op of operator * expr * expr
  | If of expr * block * block
  | Fn_lit of expr list * block
  | Call of expr * expr list
[@@deriving sexp_of]

let int = tag Token.Int |> map ~f:(fun a -> Int_lit (Int.of_string a))
let ident = tag Token.Ident |> map ~f:(fun a -> Ident a)
let string = tag Token.String |> map ~f:(fun a -> String_lit a)
let btrue = tag Token.True |> map ~f:(fun _ -> Bool_lit true)
let bfalse = tag Token.False |> map ~f:(fun _ -> Bool_lit false)

(* OPERATOR PRECEDENCE: *)
module Prec = struct
  let lowest = 0
  let equals = 1
  let lesgre = 2
  let addsub = 3
  let muldiv = 4
  let prefix = 5
  let fncall = 6
  let index = 7

  let get tok =
    let open Token in
    match tok.toktype with
    | Plus | Minus -> addsub
    | Asterisk | Slash -> muldiv
    | Lt | Gt -> lesgre
    | Equals | Not_equals -> equals
    | Lparen -> fncall
    | Lbracket -> index
    | _ -> lowest
  ;;
end

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
        (not (Token.equal curr_token Token.Semicolon))
        && minp < Prec.get curr_token
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
  (tag Token.Minus *> expression ~minp:Prec.prefix
  |> map ~f:(fun a -> Un_op (Neg, a))
  )
    input

and bang_pre input =
  (tag Token.Bang *> expression ~minp:Prec.prefix
  |> map ~f:(fun a -> Un_op (Not, a))
  )
    input

and add lhs input =
  (tag Token.Plus *> expression ~minp:Prec.addsub
  |> map ~f:(fun a -> Bin_op (Add, lhs, a))
  )
    input

and sub lhs input =
  (tag Token.Minus *> expression ~minp:Prec.addsub
  |> map ~f:(fun a -> Bin_op (Sub, lhs, a))
  )
    input

and mul lhs input =
  (tag Token.Asterisk *> expression ~minp:Prec.muldiv
  |> map ~f:(fun a -> Bin_op (Mul, lhs, a))
  )
    input

and div lhs input =
  (tag Token.Slash *> expression ~minp:Prec.muldiv
  |> map ~f:(fun a -> Bin_op (Div, lhs, a))
  )
    input

and equ lhs input =
  (tag Token.Equals *> expression ~minp:Prec.equals
  |> map ~f:(fun a -> Bin_op (Equ, lhs, a))
  )
    input

and neq lhs input =
  (tag Token.Not_equals *> expression ~minp:Prec.equals
  |> map ~f:(fun a -> Bin_op (Neq, lhs, a))
  )
    input

and les lhs input =
  (tag Token.Lt *> expression ~minp:Prec.lesgre
  |> map ~f:(fun a -> Bin_op (Les, lhs, a))
  )
    input

and gre lhs input =
  (tag Token.Gt *> expression ~minp:Prec.lesgre
  |> map ~f:(fun a -> Bin_op (Gre, lhs, a))
  )
    input

and grouped input =
  (tag Token.Lparen *> expression ~minp:Prec.lowest <* tag Token.Rparen) input

and ifp input =
  (tag Token.If *> expression ~minp:Prec.lowest
  <*> block
  <* tag Token.Else
  <*> block
  |> map ~f:(fun ((cond, conseq), alt) -> If (cond, conseq, alt))
  )
    input

and array input =
  (tag Token.Lbracket
   *> delimited (tag Token.Comma) (expression ~minp:Prec.lowest)
  <* tag Token.Rbracket
  |> map ~f:(fun a -> Array_lit a)
  )
    input

and index lhs input =
  (tag Token.Lbracket *> expression ~minp:Prec.lowest
  <* tag Token.Rbracket
  |> map ~f:(fun a -> Index (lhs, a))
  )
    input

and block input = (tag Token.Lbrace *> many statement <* tag Token.Rbrace) input

and let_s input =
  (tag Token.Let *> ident
  <* tag Token.Assign
  <*> expression ~minp:Prec.lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun (a, b) -> Let (a, b))
  )
    input

and return_s input =
  (tag Token.Return *> expression ~minp:Prec.lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Return a)
  )
    input

and expression_s input =
  (expression ~minp:Prec.lowest <* optional (tag Token.Semicolon)) input

and statement input = (let_s <|> return_s <|> expression_s) input

and func input =
  (tag Token.Function *> tag Token.Lparen *> delimited (tag Token.Comma) ident
  <* tag Token.Rparen
  <*> block
  |> map ~f:(fun (a, b) -> Fn_lit (a, b))
  )
    input

and call lhs input =
  (tag Token.Lparen *> delimited (tag Token.Comma) (expression ~minp:Prec.lowest)
  <* tag Token.Rparen
  |> map ~f:(fun a -> Call (lhs, a))
  )
    input

and hash input : expr pResult =
  let pairs =
    expression ~minp:Prec.lowest
    <*> tag Token.Colon *> expression ~minp:Prec.lowest
  in
  (tag Token.Lbrace *> delimited (tag Token.Comma) pairs
  <* tag Token.Rbrace
  |> map ~f:(fun a -> Hash_lit a)
  )
    input

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
  | If -> Some ifp
  | Function -> Some func
  | String -> Some string
  | Lbracket -> Some array
  | Lbrace -> Some hash
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
  | Lparen -> Some call
  | Lbracket -> Some index
  | _ -> None
;;

let let_s =
  tag Token.Let *> ident
  <* tag Token.Assign
  <*> expression ~minp:Prec.lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun (a, b) -> Let (a, b))
;;

let return_s : expr parser =
  tag Token.Return *> expression ~minp:Prec.lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Return a)
;;

let expression_s : expr parser =
  expression ~minp:Prec.lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> a)
;;

let statement = let_s <|> return_s <|> expression_s
let parse ~using:(p : 'a parser) str = str |> Lexer.of_string |> p
