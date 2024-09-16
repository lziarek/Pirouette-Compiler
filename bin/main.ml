let usage_msg = "USAGE: pirc <file> [-pprint] [-json] [-dot]"
let pprint_flag = ref false
let json_flag = ref false
let dot_flag = ref false
let file_ic = ref None
let basename = ref "choreo"

let anon_fun filename =
  basename := Filename.remove_extension filename;
  file_ic := Some (open_in filename)
;;

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-pprint"
    , Arg.Set pprint_flag, "Pretty print the AST" )
  ; ( "-json"
    , Arg.Set json_flag, "Output the AST in JSON format" )
  ; ( "-dot"
    , Arg.Set dot_flag, "Output the AST in DOT format" )
  ]
;;

let dot_to_file outfile_name dot_code =
  let file_oc = open_out (outfile_name ^ ".dot") in
  Printf.fprintf file_oc "%s" dot_code;
  close_out file_oc

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.parse_program lexbuf in
  if !pprint_flag then (
    Ast_utils.pprint_choreo_ast (open_out (!basename ^ ".ast")) program
  );
  if !json_flag then (
    Ast_utils.jsonify_choreo_ast (open_out (!basename ^ ".json")) program
  );
  if !dot_flag then (
    let dot_code = Ast_utils.dot_graph program in
    dot_to_file !basename dot_code
  );
  if !pprint_flag || !json_flag then (
    let locs = Ast_utils.extract_locs program in
    List.iter 
      (fun loc ->
        let ir = Irgen.epp program loc in
        if !json_flag then
          Ast_utils.jsonify_net_ast (open_out (!basename ^ "." ^ loc ^ ".json")) ir;
        if !pprint_flag then
          Ast_utils.pprint_net_ast (open_out (!basename ^ "." ^ loc ^ ".ast")) ir)
    locs;
  )
;;
