open Parsing.Interface
open Pirouettedot.Interface

let dot_to_file filename dot_code =
  let file_oc = open_out filename in
  Printf.fprintf file_oc "%s" dot_code;
  close_out file_oc

let () =
  if Array.length Sys.argv < 3 then (
    print_endline "Usage: <executable> <sourcefile> <destfile>";
    exit 1) 
  else
    let in_filename = Sys.argv.(1) in
    let file_ic = open_in in_filename in
    let lexbuf = Lexing.from_channel file_ic in
    let program = parse_program lexbuf in
    close_in file_ic;
    let dot_code = generate_dot_code program in
    let out_filename = Sys.argv.(2) ^ ".dot" in
    dot_to_file out_filename dot_code
