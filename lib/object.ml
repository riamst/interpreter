open Core
open Parser

type builtin_func =
  | Push
  | Cons
  | Len
  | First
  | Last
  | Rest
  | Puts
[@@deriving sexp_of]

type t =
  | Int_val of int
  | Bool_val of bool
  | String_val of string
  | Array_val of t list
  | Hash_val of (t, t) Hashtbl.t
  | Null
  | Function of expr list * block * (env[@sexp.opaque])
  | Builtin of builtin_func

and env = {
  store: (string, t) Hashtbl.t;
  outer: (string, t) Hashtbl.t option;
}
[@@deriving sexp_of]

let rec to_string t =
  match t with
  | Int_val a -> Int.to_string a
  | Bool_val a -> Bool.to_string a
  | String_val a -> a
  | Array_val a -> List.to_string ~f:to_string a
  | Hash_val a ->
    Hashtbl.to_alist a
    |> List.to_string ~f:(fun (a, b) -> to_string a ^ ":" ^ to_string b)
  | Null -> ""
  | Function _ -> "<func>"
  | Builtin _ -> "<built_in>"
;;

let new_global () = { store = Hashtbl.Poly.create (); outer = None }

let new_local { store; _ } =
  { store = Hashtbl.Poly.create (); outer = Some (Hashtbl.copy store) }
;;

let get { store; outer } key =
  match Hashtbl.find store key with
  | Some a -> Some a
  | None -> Option.bind ~f:(fun a -> Hashtbl.find a key) outer
;;

let set { store; _ } ~key ~data = Hashtbl.set store ~key ~data
