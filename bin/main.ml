open Interpreter
open Core

let input =
  "let five = 5;\n\
   let ten = 10;\n\
   let add = fn(x, y) {\n\
   x + y;\n\
   };\n\
   let result = add(five, ten);\n\
   !-/*5;\n\
   5 < 10 > 5;\n\
   if (5 < 10) {\n\
   return true;\n\
   } else {\n\
   return false;\n\
   }\n\
   10 == 10;\n\
   10 != 9;"
;;

let repl () =
  while true do
    printf "%s" "> ";
    Out_channel.flush stdout;
    let input =
      try
        let a = In_channel.input_line In_channel.stdin |> Option.value_exn in
        if String.equal a "exit" then failwith "exiting" else a
      with _ -> printf "%s\n" "Exiting"; exit 0
    in
    let open Parser in
    parse ~using:statement input |> fun parsed ->
    match parsed with
    | Ok (_, a) ->
      printf "%s\n" (Parser.sexp_of_expr a |> Sexp.to_string_hum);
      printf "%s\n" (Eval.eval a |> Eval.sexp_of_value |> Sexp.to_string_hum)
    | Error e -> printf "%s\n" e
  done
;;

let () =
  (* let tokens = Lexer.of_string input in *)
  (* List.iter *)
  (*   ~f:(fun a -> *)
  (*     a |> Lexer.Token.sexp_of_t |> Sexp.to_string_hum |> print_endline *)
  (*   ) *)
  (*   tokens; *)
  (* let open Parser in *)
  (* tokens *)
  (* |> many statement *)
  (* |> sexp_of_pResult (sexp_of_list sexp_of_statement) *)
  (* |> Sexp.to_string_hum *)
  (* |> print_endline; *)
  repl ()
;;
