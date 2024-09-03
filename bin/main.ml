open Ast.Interface
open Parsing.Interface

let prettyprint_flag = ref false
let dot_flag = ref false
let metainfo_flag = ref false
let input_file = ref ""
let outfile_name = ref ""
let usage_msg = "Usage: <executable> [options] <sourcefile> [-o <outputfile>]"
let read_filename filename = input_file := filename

let speclist = [
  ("-p", Arg.Set prettyprint_flag, "Pretty print the parsed program");
  ("-d", Arg.Set dot_flag, "Output the parsed program in DOT format");
  ("-m", Arg.Set metainfo_flag, "Output the parsed program with line numbers");
  ("-o", Arg.Set_string outfile_name, "Output file for the generated code");
]

let dot_to_file outfile_name dot_code =
  let file_oc = open_out (outfile_name ^ ".dot") in
  Printf.fprintf file_oc "%s" dot_code;
  close_out file_oc

let () =
  Arg.parse speclist read_filename usage_msg;
  if !input_file = "" then (
    print_endline usage_msg;
    exit 1);

  let file_ic = open_in !input_file in
  let lexbuf = Lexing.from_channel file_ic in
  let program = parse_program lexbuf in
  (* print_endline (dump_choreo_ast program); *)

  if !outfile_name <> "" then (
    (*if !prettyprint_flag then (
      let file_oc = open_out (!outfile_name (*^ ".extension"*)) in
        pretty_print (Format.formatter_of_out_channel file_oc) program;
        close_out file_oc); *)
    if !dot_flag then (
      let dot_code = dot_graph program in
      dot_to_file !outfile_name dot_code
      );
    (* if !metainfo_flag then print_metainfo program to out_file *)
  ) else (
    (*if !prettyprint_flag then pretty_print Format.std_formatter program; *)
    if !dot_flag then (print_endline (dot_graph program))
    (* if !metainfo_flag then print_metainfo program to terminal *)
  );
  close_in file_ic;
  