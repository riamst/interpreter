open Core
open Parser

let to_native_bool = function
  | Object.Bool_val a -> a
  | _ -> failwith "type error in condition"
;;

let eval_bin_op op lhs rhs =
  let open Object in
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
       for _ = 1 to rhs do
         str := !str ^ lhs
       done;
       !str
      )
  | Mul, Int_val lhs, String_val rhs ->
    String_val
      (let str = ref rhs in
       for _ = 1 to lhs do
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
  let open Object in
  match (op, rhs) with
  | Not, Bool_val a -> Bool_val (not a)
  | Neg, Int_val a -> Int_val (-a)
  | _ -> failwith "Error: type error"
;;

let rec eval env expr =
  let open Object in
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
    printf "var: %s = %s\n" name (data |> to_string);
    set env ~key:name ~data;
    Null
  | Return a -> eval env a
  | Int_lit a -> Int_val a
  | Bool_lit a -> Bool_val a
  | Hash_lit a ->
    let a = List.map ~f:(fun (key, data) -> (eval env key, eval env data)) a in
    let h =
      List.fold ~init:(Hashtbl.Poly.create ())
        ~f:(fun acc (a, b) ->
          Hashtbl.set ~key:a ~data:b acc;
          acc
        )
        a
    in
    Hash_val h
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
    | Hash_val a, b -> Hashtbl.find a b |> Option.value ~default:Null
    | _ -> failwith "notpog"
  )
  | Call (fn, args) ->
    let fn' = eval env fn in
    let args' = List.map ~f:(fun a -> eval env a) args in
    apply_function fn' args'
  | _ -> failwith "Error: Not evaluatable"

and eval_block env block =
  let open Object in
  List.fold_until ~init:Null
    ~f:(fun _ e ->
      match e with
      | Return ex -> Stop (eval env ex)
      | ex -> Continue (eval env ex)
    )
    ~finish:(fun a -> a)
    block

and apply_function fn args =
  let open Object in
  match fn with
  | Function (params, body, fn_env) ->
    let env = new_local fn_env in
    List.iter
      ~f:(fun (key, data) ->
        match key with
        | Ident str -> set env ~key:str ~data
        | _ -> failwith "(apply function) unreachable"
      )
      ( try List.zip_exn params args
        with _ -> failwith "Unexpected number of arguments"
      );
    eval_block env body
  | Builtin builtin_fn -> builtin_eval builtin_fn args
  | _ -> failwith "Error: tried to call a non-function"

and builtins a : Object.t option =
  let open Object in
  ( match a with
  | "len" -> Some Len
  | "first" -> Some First
  | "last" -> Some Last
  | "rest" -> Some Rest
  | "cons" -> Some Cons
  | "push" -> Some Push
  | "puts" -> Some Puts
  | _ -> None
  )
  |> Option.map ~f:(fun a -> Builtin a)

and builtin_eval a : Object.t list -> Object.t =
  let open Object in
  match a with
  | Puts ->
    fun alist ->
      List.iter ~f:(fun a -> a |> to_string |> print_endline) alist;
      Null
  | Len -> (
    fun a ->
      match a with
      | [ String_val str ] -> Int_val (String.length str)
      | [ Array_val a ] -> Int_val (List.length a)
      | [ _ ] ->
        failwith
          "Unexpected argument type for len function, expected string or array"
      | x ->
        failwith
          (Printf.sprintf "Unexpected number of arguments; expected 1, found %s"
             (List.length x |> Int.to_string)
          )
  )
  | First -> (
    fun a ->
      match a with
      | [ Array_val a ] -> List.hd a |> Option.value ~default:Null
      | [ _ ] ->
        failwith "Unexpected argument type for first function, expected array"
      | x ->
        failwith
          (Printf.sprintf "Unexpected number of arguments; expected 1, found %s"
             (List.length x |> Int.to_string)
          )
  )
  | Last -> (
    fun a ->
      match a with
      | [ Array_val a ] -> List.last a |> Option.value ~default:Null
      | [ _ ] ->
        failwith "Unexpected argument type for last function, expected array"
      | x ->
        failwith
          (Printf.sprintf "Unexpected number of arguments; expected 1, found %s"
             (List.length x |> Int.to_string)
          )
  )
  | Rest -> (
    fun a ->
      match a with
      | [ Array_val a ] ->
        List.tl a
        |> Option.map ~f:(fun a -> Array_val a)
        |> Option.value ~default:Null
      | [ _ ] ->
        failwith "Unexpected argument type for rest function, expected array"
      | x ->
        failwith
          (Printf.sprintf "Unexpected number of arguments; expected 1, found %s"
             (List.length x |> Int.to_string)
          )
  )
  | Cons -> (
    fun a ->
      match a with
      | [ Array_val a; b ] -> Array_val (b :: a)
      | [ _ ] ->
        failwith "Unexpected argument type for cons function, expected array"
      | x ->
        failwith
          (Printf.sprintf
             "Unexpected number of arguments for cons function; expected 2, \
              found %s"
             (List.length x |> Int.to_string)
          )
  )
  | Push -> (
    fun a ->
      match a with
      | [ Array_val a; b ] -> Array_val (a @ [ b ])
      | [ _ ] ->
        failwith "Unexpected argument type for push function, expected array"
      | x ->
        failwith
          (Printf.sprintf
             "Unexpected number of arguments for push function; expected 2, \
              found %s"
             (List.length x |> Int.to_string)
          )
  )
;;
