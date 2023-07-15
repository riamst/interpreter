open Interpreter
open Core

let input =
  "let five = 5;\n\
  \   let ten = 10;\n\
  \   let add = fn(x, y) {\n\
  \     x + y;\n\
  \   };\n\
  \   let result = add(five, ten);\n\
  \   !-/*5;\n\
  \   5 < 10 > 5;\n\
  \   if (5 < 10) {\n\
  \   return true;\n\
  \   } else {\n\
  \   return false;\n\
  \   }\n\
  \   10 == 10;\n\
  \   10 != 9;"
;;

let () =
  let lexer = Lexer.of_string input in
  Sequence.iter
    ~f:(fun a -> a |> Lexer.sexp_of_token |> Sexp.to_string_hum |> print_endline)
    lexer
;;
