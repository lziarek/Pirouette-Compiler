let extract_locs (Ast.Choreo.Prog (stmts, _)) =
  Ast_locs.extract_stmt_block stmts |> Ast_locs.LocSet.elements
;;

let jsonify_choreo_ast channel (Ast.Choreo.Prog (prog, _)) =
  Yojson.Basic.pretty_to_channel channel (Jsonify_ast.jsonify_choreo_stmt_block prog)
;;

let jsonify_net_ast channel (Ast.Net.Prog prog) =
  Yojson.Basic.pretty_to_channel channel (Jsonify_ast.jsonify_net_stmt_block prog)
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
let dot_graph (prog : Ast.Choreo.program) : string =
  Choreo_dot.generate_dot_code prog

let dot_choreo_ast channel (Ast.Choreo.Prog prog) =
  let dot_code = dot_graph (Ast.Choreo.Prog prog) in
  output_string channel dot_code;
  flush channel
;;