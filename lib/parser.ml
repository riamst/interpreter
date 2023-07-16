open Core
open Lexer

type 'a pResult = (Token.t list * 'a, string) Result.t [@@deriving sexp_of]
type 'a parser = Token.t list -> 'a pResult

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
[@@deriving sexp]

type statement =
  | Let of expr * expr
  | Return of expr
  | Expression of expr

and block = statement list

and expr =
  | Int_lit of int
  | Bool_lit of bool
  | Ident of string
  | Un_op of operator * expr
  | Bin_op of operator * expr * expr
  | If of expr * block * block
  | Fn_lit of expr list * block
  | Call of expr * expr list
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

let delimited (delim : 'a parser) (elem : 'b parser) : 'b list parser =
  elem <*> many (delim *> elem) |> map ~f:(fun (a, b) -> a :: b)
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
  | Lparen -> fncall
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
  |> map ~f:(fun a -> Un_op (Neg, a))
  )
    input

and bang_pre input =
  (tag Token.Bang *> expression ~minp:prefix |> map ~f:(fun a -> Un_op (Not, a)))
    input

and add lhs input =
  (tag Token.Plus *> expression ~minp:addsub
  |> map ~f:(fun a -> Bin_op (Add, lhs, a))
  )
    input

and sub lhs input =
  (tag Token.Minus *> expression ~minp:addsub
  |> map ~f:(fun a -> Bin_op (Sub, lhs, a))
  )
    input

and mul lhs input =
  (tag Token.Asterisk *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Mul, lhs, a))
  )
    input

and div lhs input =
  (tag Token.Slash *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Div, lhs, a))
  )
    input

and equ lhs input =
  (tag Token.Equals *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Equ, lhs, a))
  )
    input

and neq lhs input =
  (tag Token.Not_equals *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Neq, lhs, a))
  )
    input

and les lhs input =
  (tag Token.Lt *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Les, lhs, a))
  )
    input

and gre lhs input =
  (tag Token.Gt *> expression ~minp:muldiv
  |> map ~f:(fun a -> Bin_op (Gre, lhs, a))
  )
    input

and grouped input =
  (tag Token.Lparen *> expression ~minp:lowest <* tag Token.Rparen) input

and ifp input =
  (tag Token.If *> expression ~minp:lowest
  <*> block
  <* tag Token.Else
  <*> block
  |> map ~f:(fun ((cond, conseq), alt) -> If (cond, conseq, alt))
  )
    input

and block input = (tag Token.Lbrace *> many statement <* tag Token.Rbrace) input

and let_s input =
  (tag Token.Let *> ident
  <* tag Token.Assign
  <*> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun (a, b) -> Let (a, b))
  )
    input

and return_s input =
  (tag Token.Return *> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Return a)
  )
    input

and expression_s input =
  (expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Expression a)
  )
    input

and statement input = (let_s <|> return_s <|> expression_s) input

and func input =
  (tag Token.Function *> tag Token.Lparen *> delimited (tag Token.Comma) ident
  <* tag Token.Rparen
  <*> block
  |> map ~f:(fun (a, b) -> Fn_lit (a, b))
  )
    input

and call lhs input =
  (tag Token.Lparen *> delimited (tag Token.Comma) (expression ~minp:lowest)
  <* tag Token.Rparen
  |> map ~f:(fun a -> Call (lhs, a))
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
  | a ->
    printf "Error no prefix: %s\n"
      (a |> Token.sexp_of_tokentype |> Sexp.to_string_hum);
    None

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
  | a ->
    printf "Error no infix: %s\n"
      (a |> Token.sexp_of_tokentype |> Sexp.to_string_hum);
    None
;;

let let_s : statement parser =
  tag Token.Let *> ident
  <* tag Token.Assign
  <*> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun (a, b) -> Let (a, b))
;;

let return_s : statement parser =
  tag Token.Return *> expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Return a)
;;

let expression_s : statement parser =
  expression ~minp:lowest
  <* optional (tag Token.Semicolon)
  |> map ~f:(fun a -> Expression a)
;;

let statement = let_s <|> return_s <|> expression_s
let parse ~using:(p : 'a parser) str = str |> Lexer.of_string |> p
