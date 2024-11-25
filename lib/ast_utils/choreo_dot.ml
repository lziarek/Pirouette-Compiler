module Choreo = Parsing.Parsed_ast.Choreo
open Local_dot

let string_of_pos = Parsing.Parsed_ast.Pos_info.string_of_pos
let spf = Printf.sprintf

(** [dot_choreo_type typ] creates the dot code for choreo types [typ]

    - [typ] is a choreo type.
    - Variants of choreo types include TUnit, TLoc, TSend, TProd, and TSum.
    - For Variant [TLoc], it calls helper function [dot_local_type] to generate the dot code for the local type.
    - For variants [TSend], [TProd], and [TSum], it calls helper function [dot_choreo_type] to generate the dot code for the choreo type.
    - Connects the choreo type node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the choreo type [typ] and the
      second element is the node name of the choreo type [typ]. *)
let rec dot_choreo_type (typ : Choreo.typ) : string * string =
  let node_name = generate_node_name () in
  match typ with
  | TUnit pos -> spf "%s [label=\"() %s\"];\n" node_name (string_of_pos pos), node_name
  | TLoc (LocId (id, _), typ, pos) ->
    let locid_node = spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos) in
    let c, n = dot_local_type typ in
    let edge = spf "%s -> %s;\n" node_name n in
    locid_node ^ edge ^ c, node_name
  | TVar (Typ_Id (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | TMap (typ1, typ2, pos) ->
    let c1, n1 = dot_choreo_type typ1 in
    let c2, n2 = dot_choreo_type typ2 in
    let send_node = spf "%s [label=\"Send %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    send_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | TProd (typ1, typ2, pos) ->
    let c1, n1 = dot_choreo_type typ1 in
    let c2, n2 = dot_choreo_type typ2 in
    let prod_node = spf "%s [label=\"Product %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    prod_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | TSum (typ1, typ2, pos) ->
    let c1, n1 = dot_choreo_type typ1 in
    let c2, n2 = dot_choreo_type typ2 in
    let sum_node = spf "%s [label=\"Sum %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    sum_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
;;

(** [dot_pattern pat] creates the dot code for patterns [pat]

    - [pat] is a pattern.
    - Variants of patterns include Default, Var, Pair, LocPat, Left, and Right.
    - For variant [LocPat], it calls helper function [dot_local_pattern] to generate the dot code for the local pattern.
    - For variants [Pair], [Left], and [Right], it calls helper function [dot_pattern] to generate the dot code for the pattern.
    - Connects the pattern node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the pattern [pat] and the
      second element is the node name of the pattern [pat]. *)
let rec dot_choreo_pattern (pat : Choreo.pattern) : string * string =
  let node_name = generate_node_name () in
  match pat with
  | Default pos ->
    spf "%s [label=\"Default %s\"];\n" node_name (string_of_pos pos), node_name
  | Var (VarId (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | Pair (pat1, pat2, pos) ->
    let c1, n1 = dot_choreo_pattern pat1 in
    let c2, n2 = dot_choreo_pattern pat2 in
    let pair_node = spf "%s [label=\"Pair %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | LocPat (LocId (loc, _), lp, pos) ->
    let locid_node = spf "%s [label=\"%s %s\"];\n" node_name loc (string_of_pos pos) in
    let c, n = dot_local_pattern lp in
    let edge = spf "%s -> %s;\n" node_name n in
    locid_node ^ edge ^ c, node_name
  | Left (pat, pos) ->
    let c, n = dot_choreo_pattern pat in
    let left_node = spf "%s [label=\"Left %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (pat, pos) ->
    let c, n = dot_choreo_pattern pat in
    let right_node = spf "%s [label=\"Right %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name
;;

(** [dot_stmts stmts] creates the dot code for a list of statements [stmts]

    - [stmts] is a list of statements.
    - Calls helper function [dot_stmt] on each statement in [stmts] and concatenates the dot code for each statement.
    - Recursively calls [dot_stmts] on the rest of the list of statements [stmts].
    - Returns: A tuple of strings where the first element is the dot code for the list of statements [stmts] and the
      second element is the node name of the list of statements [stmts]. *)
let rec dot_stmts (stmts : Choreo.stmt_block) : string * string =
  match stmts with
  | [] -> "", ""
  | stmt :: rest ->
    let stmt_dot_code, stmt_node_name = dot_stmt stmt in
    let rest_dot_code, rest_node_name = dot_stmts rest in
    ( (stmt_dot_code ^ if rest <> [] then "\n" ^ rest_dot_code else rest_dot_code)
    , stmt_node_name ^ " " ^ rest_node_name )

(** [dot_stmt statement] creates the dot code for statements [statement]

    - [statement] is a statement.
    - Variants of statements include Decl, Assign, TypeDecl.
    - Calls helper functions [dot_pattern], [dot_choreo_type], and [dot_local_pattern] to generate the dot code for the statement.
    - Connects the statement node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the statement [statement] and the
      second element is the node name of the statement [statement]. *)
and dot_stmt (stmt : Choreo.stmt) : string * string =
  let node_name = generate_node_name () in
  match stmt with
  | Decl (pat, typ, pos) ->
    let c1, n1 = dot_choreo_pattern pat in
    let c2, n2 = dot_choreo_type typ in
    let decl_node = spf "%s [label=\"Decl %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    decl_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Assign (pat_list, expr, pos) ->
    let rec pattern_loop patterns =
      match patterns with
      | [] -> "", ""
      | pat :: rest ->
        let pat_c, pat_n = dot_choreo_pattern pat in
        let edge = spf "%s -> %s;\n" node_name pat_n in
        let rest_c, rest_e = pattern_loop rest in
        pat_c ^ rest_c, edge ^ rest_e
    in
    let pattern_code, pattern_edge = pattern_loop pat_list in
    let c1, n1 = dot_choreo_expr expr in
    let assign_node = spf "%s [label=\"Assign %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    assign_node ^ pattern_edge ^ edge1 ^ pattern_code ^ c1, node_name
  | TypeDecl (TypId (id, _), typ, pos) ->
    let var_node = spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos) in
    let c, n = dot_choreo_type typ in
    let edge = spf "%s -> %s;\n" node_name n in
    var_node ^ edge ^ c, node_name

(** [dot_choreo_expr chor_expr] creates the dot code for choreo expressions [chor_expr]

    - [chor_expr] is a choreo expression.
    - Variants of choreo expressions include Unit, Var, LocExpr, Send, Sync, If, Let, FunDef, FunApp, Pair, Fst, Snd, Left, Right, and Match.
    - Calls helper functions [dot_local_expr], [dot_choreo_expr], [dot_stmts], [dot_choreo_cases], [dot_choreo_case], [dot_pattern], and [dot_choreo_expr]
      to generate the dot code for the choreo expression.
    - Connects the choreo expression node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the choreo expression [chor_expr] and the
      second element is the node name of the choreo expression [chor_expr]. *)
and dot_choreo_expr (expr : Choreo.expr) : string * string =
  let node_name = generate_node_name () in
  match expr with
  | Unit pos -> spf "%s [label=\"() %s\"];\n" node_name (string_of_pos pos), node_name
  | Var (VarId (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | LocExpr (LocId (id, _), le, pos) ->
    let locid_node = spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos) in
    let c, n = dot_local_expr le in
    let edge = spf "%s -> %s;\n" node_name n in
    locid_node ^ edge ^ c, node_name
  | Send (LocId (id1, _), expr, LocId (id2, _), pos) ->
    let send_node1 =
      spf "%s [label=\"Send from: %s %s\"];\n" node_name id1 (string_of_pos pos)
    in
    let c, n = dot_choreo_expr expr in
    let send_node2 =
      spf "%s [label=\"Send to: %s %s\"];\n" node_name id2 (string_of_pos pos)
    in
    let edge = spf "%s -> %s;\n" node_name n in
    send_node1 ^ send_node2 ^ edge ^ c, node_name
  | Sync (LocId (loc_id1, _), LabelId (label, _), LocId (loc_id2, _), expr, pos) ->
    let c, n = dot_choreo_expr expr in
    let sync_node =
      spf
        "%s [label=\"Sync: %s[%s] -> %s %s\"];\n"
        node_name
        loc_id1
        label
        loc_id2
        (string_of_pos pos)
    in
    let edge = spf "%s -> %s;\n" node_name n in
    sync_node ^ edge ^ c, node_name
  | If (cond, then_expr, else_expr, pos) ->
    let c1, n1 = dot_choreo_expr cond in
    let c2, n2 = dot_choreo_expr then_expr in
    let c3, n3 = dot_choreo_expr else_expr in
    let if_node = spf "%s [label=\"If %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    let edge3 = spf "%s -> %s;\n" node_name n3 in
    if_node ^ edge1 ^ edge2 ^ edge3 ^ c1 ^ c2 ^ c3, node_name
  | Let (decl_block, expr, pos) ->
    let c1, n1 = dot_stmts decl_block in
    let c2, n2 = dot_choreo_expr expr in
    let let_node = spf "%s [label=\"Let %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    let_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | FunDef (pat_list, expr, pos) ->
    let rec pattern_loop patterns =
      match patterns with
      | [] -> "", ""
      | pat :: rest ->
        let pat_c, pat_n = dot_choreo_pattern pat in
        let edge = spf "%s -> %s;\n" node_name pat_n in
        let rest_c, rest_e = pattern_loop rest in
        pat_c ^ rest_c, edge ^ rest_e
    in
    let pattern_code, pattern_edge = pattern_loop pat_list in
    let c1, n1 = dot_choreo_expr expr in
    let fundef_node = spf "%s [label=\"FunDef %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    fundef_node ^ pattern_edge ^ edge1 ^ pattern_code ^ c1, node_name
  | FunApp (f, arg, pos) ->
    let c1, n1 = dot_choreo_expr f in
    let c2, n2 = dot_choreo_expr arg in
    let funapp_node = spf "%s [label=\"FunApp %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    funapp_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Pair (e1, e2, pos) ->
    let c1, n1 = dot_choreo_expr e1 in
    let c2, n2 = dot_choreo_expr e2 in
    let pair_node = spf "%s [label=\"Pair %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Fst (e, pos) ->
    let c, n = dot_choreo_expr e in
    let fst_node = spf "%s [label=\"Fst %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    fst_node ^ edge ^ c, node_name
  | Snd (e, pos) ->
    let c, n = dot_choreo_expr e in
    let snd_node = spf "%s [label=\"Snd %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    snd_node ^ edge ^ c, node_name
  | Left (e, pos) ->
    let c, n = dot_choreo_expr e in
    let left_node = spf "%s [label=\"Left %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (e, pos) ->
    let c, n = dot_choreo_expr e in
    let right_node = spf "%s [label=\"Right %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name
  | Match (e, cases, pos) ->
    (* [dot_choreo_case (pat, expr)] creates the dot code for a choreo case [(pat, expr)]

       - [(pat, expr)] is a tuple of a choreo pattern and a choreo expression.
       - Calls [dot_pattern] on the choreo pattern and [dot_choreo_expr] on the choreo expression.
       - Connects the [dot_choreo_case] node to it's children, the choreo pattern and choreo expression nodes.
       - Returns: A tuple of strings where the first element is the dot code for the choreo case [(pat, expr)]
         and the second element is the node name of the choreo case [(pat, expr)]. *)
    let[@inline] dot_choreo_case (pat, expr) =
      let node_name = generate_node_name () in
      let c1, pat_name = dot_choreo_pattern pat in
      let c2, expr_name = dot_choreo_expr expr in
      let case_node = spf "%s [label=\"Case\"];\n" node_name in
      let edge1 = spf "%s -> %s;\n" node_name pat_name in
      let edge2 = spf "%s -> %s;\n" node_name expr_name in
      case_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
    in
    (* [dot_choreo_cases cases] calls [dot_choreo_case] on each choreo case in [cases] and creates a tuple.

       - [cases] is a list of choreo cases.
       - Returns: Tuples of strings where the first element is the dot code for the choreo cases in [cases] and the second element is the
         node name of the choreo cases in [cases]. *)
    let[@inline] dot_choreo_cases cases =
      let case_nodes, case_node_names = List.split (List.map dot_choreo_case cases) in
      String.concat "" case_nodes, String.concat " " case_node_names
    in
    let c1, n1 = dot_choreo_expr e in
    let match_node = spf "%s [label=\"Match %s\"];\n" node_name (string_of_pos pos) in
    let c2, n2 = dot_choreo_cases cases in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    match_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
;;

(** [generate_dot_code program] generates the dot code for a choreo program [program]

    - [program] is a choreo program.
    - Calls helper function [dot_stmts] to generate the dot code for the list of statements in the program.
    - Resets the node counter to 0 so that the next call to [generate_node_name] will start from 0.
    - Returns: A string that represents the dot code for the choreo program [program]. *)
let generate_dot_code (stmt_block : Choreo.stmt_block) =
  let code, _ = dot_stmts stmt_block in
  node_counter := 0;
  spf "digraph G {\n%s\n}\n" code
;;
