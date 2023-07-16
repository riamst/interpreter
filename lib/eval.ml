open Core
open Parser

type value =
  | Int_val of int
  | Bool_val of bool
  | Unit
[@@deriving sexp]

let to_native_bool = function
  | Bool_val a -> a
  | _ -> failwith "type error in condition"
;;

let eval_bin_op op lhs rhs =
  match (op, lhs, rhs) with
  | Add, Int_val lhs, Int_val rhs -> Int_val (lhs + rhs)
  | Sub, Int_val lhs, Int_val rhs -> Int_val (lhs - rhs)
  | Mul, Int_val lhs, Int_val rhs -> Int_val (lhs * rhs)
  | Div, Int_val lhs, Int_val rhs -> Int_val (lhs / rhs)
  | Les, Int_val lhs, Int_val rhs -> Bool_val (lhs < rhs)
  | Gre, Int_val lhs, Int_val rhs -> Bool_val (lhs > rhs)
  | Equ, Int_val lhs, Int_val rhs -> Bool_val (lhs = rhs)
  | Neq, Int_val lhs, Int_val rhs -> Bool_val (lhs <> rhs)
  | _ -> failwith "type error"
;;

let eval_un_op op rhs =
  match (op, rhs) with
  | Not, Bool_val a -> Bool_val (not a)
  | Neg, Int_val a -> Int_val (-a)
  | _ -> failwith "type error"
;;

let rec eval expr =
  match expr with
  | Let (Ident name, a) ->
    printf "var: %s = %s\n" name (eval a |> sexp_of_value |> Sexp.to_string_hum);
    Unit
  | Return a -> eval a
  | Int_lit a -> Int_val a
  | Bool_lit a -> Bool_val a
  | Bin_op (op, lhs, rhs) -> eval_bin_op op (eval lhs) (eval rhs)
  | Un_op (op, rhs) -> eval_un_op op (eval rhs)
  | If (cond, conseq, alt) ->
    if eval cond |> to_native_bool then eval_block conseq else eval_block alt
  | _ -> failwith "wh"

and eval_block block =
  List.fold_until ~init:Unit
    ~f:(fun _ e ->
      match e with
      | Return ex -> Stop (eval ex)
      | ex -> Continue (eval ex)
    )
    ~finish:(fun a -> a)
    block
;;
