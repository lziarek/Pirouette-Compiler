let usage_msg = "USAGE: pirc <src_file> [-pprint] [-json] [-dot] [-o <output_file>]"
let pprint_flag = ref false
let json_flag = ref false
let dot_flag = ref false
let file_ic = ref None
let outfile_name = ref ""
let open_file_ic filename = file_ic := Some (open_in filename)

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-pprint"
    , Arg.Set pprint_flag
    , "Pretty print the parsed program and Net IR, default option if no flag provided" )
  ; "-json", Arg.Set json_flag, "Output the AST in JSON format"
  ; "-dot", Arg.Set dot_flag, "Output the AST in DOT format"
  ; ( "-o"
    , Arg.Set_string outfile_name
    , "Specify the output file name (optional). If provided, the program will \
       automatically add the .pir/.json/.dot extension" )
  ]
;;

let () =
  Arg.parse speclist open_file_ic usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    prerr_endline usage_msg;
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.parse_program lexbuf in
  if (not !pprint_flag) && (not !json_flag) && not !dot_flag then pprint_flag := true;
  if !outfile_name <> ""
  then (
    if !pprint_flag
    then Ast_utils.pprint_choreo_ast (open_out (!outfile_name ^ ".pir")) program;
    if !json_flag
    then Ast_utils.jsonify_choreo_ast (open_out (!outfile_name ^ ".json")) program;
    if !dot_flag then Ast_utils.dot_choreo_ast (open_out (!outfile_name ^ ".dot")) program;
    if !pprint_flag || !json_flag
    then (
      let locs = Ast_utils.extract_locs program in
      List.iter
        (fun loc ->
          let ir = Irgen.epp program loc in
          if !json_flag
          then
            Ast_utils.jsonify_net_ast (open_out (!outfile_name ^ "." ^ loc ^ ".json")) ir;
          if !pprint_flag
          then Ast_utils.pprint_net_ast (open_out (!outfile_name ^ "." ^ loc ^ ".pir")) ir)
        locs))
  else (
    if !pprint_flag then Ast_utils.pprint_choreo_ast stdout program;
    if !json_flag then Ast_utils.jsonify_choreo_ast stdout program;
    if !dot_flag then Ast_utils.dot_choreo_ast stdout program;
    if !pprint_flag || !json_flag
    then (
      let locs = Ast_utils.extract_locs program in
      List.iter
        (fun loc ->
          let ir = Irgen.epp program loc in
          if !json_flag then Ast_utils.jsonify_net_ast stdout ir;
          if !pprint_flag then Ast_utils.pprint_net_ast stdout ir)
        locs))
;;
