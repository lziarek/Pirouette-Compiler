open Ast.Interface
open Parsing.Interface

let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: <executable> <sourcefile>";
    exit 1)
  else
    let filename = Sys.argv.(1) in
    let file_ic = open_in filename in
    let lexbuf = Lexing.from_channel file_ic in
    let program = parse_program lexbuf in
    (*print_endline (dump_choreo_ast program)*)
      pretty_print Format.std_formatter program;
    close_in file_ic
