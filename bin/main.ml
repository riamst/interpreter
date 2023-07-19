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
  let env = Eval.Env.new_global () in
  while true do
    printf "%s" "> ";
    Out_channel.flush stdout;
    let input =
      try
        match In_channel.input_line In_channel.stdin with
        | Some "exit" -> failwith ""
        | Some a -> a
        | None -> failwith ""
      with _ -> printf "%s\n" "Exiting"; exit 0
    in
    let open Parser in
    parse ~using:(all_consuming statement) input |> fun parsed ->
    match parsed with
    | Ok (_, a) -> (
      printf "%s\n" (Parser.sexp_of_expr a |> Sexp.to_string_hum);
      try
        printf "%s\n"
          (Eval.eval env a |> Eval.Env.sexp_of_value |> Sexp.to_string_hum)
      with Failure e -> printf "%s\n" e
    )
    | Error e -> printf "Parsing Error: %s\n" e
  done
;;

let () = repl ()
