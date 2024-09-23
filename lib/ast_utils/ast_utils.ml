let extract_locs (Ast.Choreo.Prog (stmts, _)) =
  Ast_locs.extract_stmt_block stmts |> Ast_locs.LocSet.elements
;;

let stringify_jsonify_choreo_ast (Ast.Choreo.Prog (prog, _)) =
  Jsonify_ast.jsonify_choreo_stmt_block prog |> Yojson.Basic.pretty_to_string
;;

let stringify_jsonify_net_ast (Ast.Net.Prog prog) =
  Jsonify_ast.jsonify_net_stmt_block prog |> Yojson.Basic.pretty_to_string
;;

let jsonify_choreo_ast channel (Ast.Choreo.Prog (prog, _)) =
  Jsonify_ast.jsonify_choreo_stmt_block prog |> Yojson.Basic.pretty_to_channel channel
;;

let jsonify_net_ast channel (Ast.Net.Prog prog) =
  Jsonify_ast.jsonify_net_stmt_block prog |> Yojson.Basic.pretty_to_channel channel
;;

let stringify_pprint_choreo_ast (Ast.Choreo.Prog (prog, _)) =
  Pprint_ast.pprint_choreo_stmt_block Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let stringify_pprint_net_ast (Ast.Net.Prog prog) =
  Pprint_ast.pprint_net_stmt_block Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let pprint_choreo_ast channel (Ast.Choreo.Prog (prog, _)) =
  let ppf = Format.formatter_of_out_channel channel in
  Pprint_ast.pprint_choreo_stmt_block ppf prog;
  Format.pp_print_newline ppf ()
;;

let pprint_net_ast channel (Ast.Net.Prog prog) =
  let ppf = Format.formatter_of_out_channel channel in
  Pprint_ast.pprint_net_stmt_block ppf prog;
  Format.pp_print_newline ppf ()
;;

let stringify_dot_choreo_ast program = Choreo_dot.generate_dot_code program

let dot_choreo_ast channel (Ast.Choreo.Prog (prog, metainfo)) =
  let dot_code = stringify_dot_choreo_ast (Ast.Choreo.Prog (prog, metainfo)) in
  output_string channel dot_code;
  flush channel
;;
