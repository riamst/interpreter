open Core
open Lexer

type 'a pResult = (Token.t list * 'a, string) Result.t [@@deriving sexp_of]
type 'a parser = Token.t list -> 'a pResult

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

let all_consuming (p : 'a parser) : 'a parser =
 fun input ->
  match p input with
  | Error e -> Error e
  | Ok ([], parsed) -> Ok ([], parsed)
  | Ok (failtok :: _, _) ->
    Error
      (Printf.sprintf
         "(all_consuming) expected to consume the whole input, failed at: %s\n"
         (failtok |> Token.sexp_of_t |> Sexp.to_string_hum)
      )
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
  optional elem
  <*> many (delim *> elem)
  |> map ~f:(fun (a, b) ->
         match a with
         | Some x -> x :: b
         | None -> []
     )
;;
