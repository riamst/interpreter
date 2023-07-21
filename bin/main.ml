open Interpreter
open Core

let repl () =
  print_endline
    "Welcome to the Monkey REPL\n\
     Start typing in your program, or type \": load <filename>\" to load from \
     a file";
  let env = Object.new_global () in
  while true do
    print_string "> ";
    Out_channel.flush stdout;
    let input =
      try
        match In_channel.input_line In_channel.stdin with
        | Some "exit" -> failwith ""
        | Some a when String.is_prefix ~prefix:":load " a -> (
          let filename = String.chop_prefix_if_exists ~prefix:":load " a in
          try In_channel.read_all filename with _ -> failwith ""
        )
        | Some a -> a
        | None -> failwith ""
      with _ -> print_endline "Exiting"; exit 0
    in
    let open Parser in
    match parse ~using:(Parsik.all_consuming statement) input with
    | Ok (_, a) -> (
      (* printf "%s\n" (Parser.sexp_of_expr a |> Sexp.to_string_hum); *)
      try
        (* printf "%s\n" *)
        (*   (Eval.eval env a |> Eval.Env.sexp_of_t |> Sexp.to_string_hum); *)
        print_endline (Eval.eval env a |> Object.to_string)
      with Failure e -> print_endline e
    )
    | Error e -> printf "Parsing Error: %s\n" e
  done
;;

let () = repl ()
