open Core
open Parser

module Env = struct
  type value =
    | Int_val of int
    | Bool_val of bool
    | String_val of string
    | Array_val of value list
    | Null
    | Function of expr list * block * (t[@sexp.opaque])
    | Builtin of (value list -> value)

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
  | Add, String_val lhs, String_val rhs -> String_val (lhs ^ rhs)
  | Add, String_val lhs, Int_val rhs -> String_val (lhs ^ Int.to_string rhs)
  | Add, Int_val lhs, String_val rhs -> String_val (Int.to_string lhs ^ rhs)
  | Sub, Int_val lhs, Int_val rhs -> Int_val (lhs - rhs)
  | Mul, Int_val lhs, Int_val rhs -> Int_val (lhs * rhs)
  | Mul, String_val lhs, Int_val rhs ->
    String_val
      (let str = ref lhs in
       for i = 1 to rhs do
         str := !str ^ lhs
       done;
       !str
      )
  | Mul, Int_val lhs, String_val rhs ->
    String_val
      (let str = ref rhs in
       for i = 1 to lhs do
         str := !str ^ rhs
       done;
       !str
      )
  | Div, Int_val lhs, Int_val rhs -> Int_val (lhs / rhs)
  | Les, Int_val lhs, Int_val rhs -> Bool_val (lhs < rhs)
  | Gre, Int_val lhs, Int_val rhs -> Bool_val (lhs > rhs)
  | Equ, Int_val lhs, Int_val rhs -> Bool_val (lhs = rhs)
  | Neq, Int_val lhs, Int_val rhs -> Bool_val (lhs <> rhs)
  | _ -> failwith "Error: type error"
;;

let eval_un_op op rhs =
  let open Env in
  match (op, rhs) with
  | Not, Bool_val a -> Bool_val (not a)
  | Neg, Int_val a -> Int_val (-a)
  | _ -> failwith "Error: type error"
;;

let rec eval env expr =
  let open Env in
  match expr with
  | Ident a -> (
    try
      match get env a with
      | Some value -> value
      | None -> builtins a |> Option.value_exn
    with _ -> failwith (Printf.sprintf "Error: variable %s not bound" a)
  )
  | String_lit a -> String_val a
  | Let (Ident name, a) ->
    let data = eval env a in
    printf "var: %s = %s\n" name (data |> sexp_of_value |> Sexp.to_string_hum);
    set env ~key:name ~data;
    Null
  | Return a -> eval env a
  | Int_lit a -> Int_val a
  | Bool_lit a -> Bool_val a
  | Bin_op (op, lhs, rhs) -> eval_bin_op op (eval env lhs) (eval env rhs)
  | Un_op (op, rhs) -> eval_un_op op (eval env rhs)
  | If (cond, conseq, alt) ->
    if eval env cond |> to_native_bool then eval_block env conseq
    else eval_block env alt
  | Fn_lit (params, body) -> Function (params, body, env)
  | Array_lit a -> Array_val (List.map ~f:(eval env) a)
  | Index (left, i) -> (
    match (eval env left, eval env i) with
    | Array_val a, Int_val idx -> List.nth a idx |> Option.value ~default:Null
    | _ -> failwith "notpog"
  )
  | Call (fn, args) ->
    let fn' = eval env fn in
    let args' = List.map ~f:(fun a -> eval env a) args in
    apply_function fn' args'
  | _ -> failwith "Error: Not evaluatable"

and eval_block env block =
  let open Env in
  List.fold_until ~init:Null
    ~f:(fun _ e ->
      match e with
      | Return ex -> Stop (eval env ex)
      | ex -> Continue (eval env ex)
    )
    ~finish:(fun a -> a)
    block

and apply_function (fn : Env.value) args : Env.value =
  match fn with
  | Function (params, body, fn_env) ->
    let env = Env.new_local fn_env in
    List.iter
      ~f:(fun (key, data) ->
        match key with
        | Ident str -> Env.set env ~key:str ~data
        | _ -> failwith "(apply function) unreachable"
      )
      ( try List.zip_exn params args
        with _ -> failwith "Unexpected number of arguments"
      );
    eval_block env body
  | Builtin builtin_fn -> builtin_fn args
  | _ -> failwith "Error: tried to call a non-function"

and builtins a =
  match a with
  | "len" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.String_val str ] -> Env.Int_val (String.length str)
           | [ Env.Array_val a ] -> Env.Int_val (List.length a)
           | [ _ ] ->
             failwith
               "Unexpected argument type for len function, expected string or \
                array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments; expected 1, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | "first" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.Array_val a ] -> List.hd a |> Option.value ~default:Env.Null
           | [ _ ] ->
             failwith
               "Unexpected argument type for first function, expected array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments; expected 1, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | "last" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.Array_val a ] ->
             List.last a |> Option.value ~default:Env.Null
           | [ _ ] ->
             failwith
               "Unexpected argument type for last function, expected array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments; expected 1, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | "rest" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.Array_val a ] ->
             List.tl a
             |> Option.map ~f:(fun a -> Env.Array_val a)
             |> Option.value ~default:Env.Null
           | [ _ ] ->
             failwith
               "Unexpected argument type for rest function, expected array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments; expected 1, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | "cons" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.Array_val a; b ] -> Env.Array_val (b :: a)
           | [ _ ] ->
             failwith
               "Unexpected argument type for cons function, expected array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments for cons function; expected \
                   2, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | "push" ->
    Some
      (Env.Builtin
         (fun a ->
           match a with
           | [ Env.Array_val a; b ] -> Env.Array_val (a @ [ b ])
           | [ _ ] ->
             failwith
               "Unexpected argument type for push function, expected array"
           | x ->
             failwith
               (Printf.sprintf
                  "Unexpected number of arguments for push function; expected \
                   2, found %s"
                  (List.length x |> Int.to_string)
               )
         )
      )
  | _ -> None
;;
