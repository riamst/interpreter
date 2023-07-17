open Core
open Parser

module Env = struct
  type value =
    | Int_val of int
    | Bool_val of bool
    | Unit
    | Function of expr list * block * (t[@sexp.opaque])

  and t = {
    store: (string, value) Hashtbl.t;
    outer: (string, value) Hashtbl.t option;
  }
  [@@deriving sexp_of]

  let new_global () = { store = Hashtbl.create (module String); outer = None }

  let new_local { store; _ } =
    {
      store = Hashtbl.create (module String);
      outer = Some (Hashtbl.copy store);
    }
  ;;

  let get { store; outer } key =
    match Hashtbl.find store key with
    | Some a -> Some a
    | None -> Option.bind ~f:(fun a -> Hashtbl.find a key) outer
  ;;

  let set { store; _ } ~key ~data = Hashtbl.set store ~key ~data
end

let to_native_bool = function
  | Env.Bool_val a -> a
  | _ -> failwith "type error in condition"
;;

let eval_bin_op op lhs rhs =
  let open Env in
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
  let open Env in
  match (op, rhs) with
  | Not, Bool_val a -> Bool_val (not a)
  | Neg, Int_val a -> Int_val (-a)
  | _ -> failwith "type error"
;;

let rec eval env expr =
  let open Env in
  match expr with
  | Ident a -> (
    try get env a |> Option.value_exn
    with _ -> failwith "Error: variable not bound"
  )
  | Let (Ident name, a) ->
    let data = eval env a in
    printf "var: %s = %s\n" name (data |> sexp_of_value |> Sexp.to_string_hum);
    set env ~key:name ~data;
    Unit
  | Return a -> eval env a
  | Int_lit a -> Int_val a
  | Bool_lit a -> Bool_val a
  | Bin_op (op, lhs, rhs) -> eval_bin_op op (eval env lhs) (eval env rhs)
  | Un_op (op, rhs) -> eval_un_op op (eval env rhs)
  | If (cond, conseq, alt) ->
    if eval env cond |> to_native_bool then eval_block env conseq
    else eval_block env alt
  | Fn_lit (params, body) -> Function (params, body, env)
  | Call (fn, args) ->
    let fn' = eval env fn in
    let args' = List.map ~f:(fun a -> eval env a) args in
    apply_function fn' args'
  | _ -> failwith "wh"

and eval_block env block =
  let open Env in
  List.fold_until ~init:Unit
    ~f:(fun _ e ->
      match e with
      | Return ex -> Stop (eval env ex)
      | ex -> Continue (eval env ex)
    )
    ~finish:(fun a -> a)
    block

and apply_function (fn : Env.value) args : Env.value =
  let open Env in
  match fn with
  | Env.Function (params, body, fn_env) ->
    let env = Env.new_local fn_env in
    List.iter
      ~f:(fun (key, data) ->
        match key with
        | Ident str -> set env ~key:str ~data
        | _ -> failwith "bigerror"
      )
      (List.zip_exn params args);
    eval_block env body
  | _ -> failwith "tried to call a non-function"
;;
