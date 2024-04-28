open Ast.Local
open Ast.Choreo

let node_counter = ref 0

(** [generate_node_name] generates a node name for the dot code

    - This function reads the current node counter, increments it, and concatentates it with the node_id to create a node name.
    - Returns: A string that represents a node name in the dot code.
*)
let generate_node_name () =
  let node_id = !node_counter in
  node_counter := !node_counter + 1;
  "n" ^ string_of_int node_id
(* node name format: n + node_counter *)

(** [generate_dot_code program] generates the dot code for a choreo program [program]
    
    - [program] is a choreo program.
    - Calls helper function [dot_stmts] to generate the dot code for the list of statements in the program.
    - Resets the node counter to 0 so that the next call to [generate_node_name] will start from 0.
    - Returns: A string that represents the dot code for the choreo program [program].
*)
let rec generate_dot_code (Ast.Choreo.Prog program) =
  let (code,_) = dot_stmts program in
  node_counter := 0;
  Printf.sprintf "digraph G {\n%s\n}\n" code

(** [dot_stmts stmts] creates the dot code for a list of statements [stmts]
        
        - [stmts] is a list of statements.
        - Calls helper function [dot_stmt] on each statement in [stmts] and concatenates the dot code for each statement.
        - Recursively calls [dot_stmts] on the rest of the list of statements [stmts].
        - Returns: A tuple of strings where the first element is the dot code for the list of statements [stmts] and the 
        second element is the node name of the list of statements [stmts].
*)
and dot_stmts (stmts : Ast.Choreo.statement list) : string * string =
  match stmts with
  | [] -> "", ""
  | stmt :: rest ->
    let (stmt_dot_code, stmt_node_name) = dot_stmt stmt in
    let (rest_dot_code, rest_node_name) = dot_stmts rest in
    (stmt_dot_code ^ (if rest <> [] then "\n" ^ rest_dot_code else rest_dot_code),
     stmt_node_name ^ " " ^ rest_node_name)

(** [dot_stmt statement] creates the dot code for statements [statement]
        
        - [statement] is a statement.
        - Variants of statements include Decl, Assign, TypeDecl.
        - Calls helper functions [dot_pattern], [dot_choreo_type], and [dot_local_pattern] to generate the dot code for the statement.
        - Connects the statement node to it's children.
        - Returns: A tuple of strings where the first element is the dot code for the statement [statement] and the 
        second element is the node name of the statement [statement].
*)
and dot_stmt statement = 
  let node_name = generate_node_name () in 
  match statement with 
  | Decl (patn, typ) -> 
      let (c1, n1) = dot_pattern patn in 
      let (c2, n2) = dot_choreo_type typ in 
      let decl_node = Printf.sprintf "%s [label=\"Decl\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (decl_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Assign (patn, expr) ->
      let (c1, n1) = dot_pattern patn in 
      let (c2, n2) = dot_choreo_expr expr in 
      let assign_node = Printf.sprintf "%s [label=\"Assign\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (assign_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | TypeDecl (VarId id, typ) ->
      let var_node = Printf.sprintf "%s [label=\"%s\"];\n" node_name id in 
      let (c, n) = dot_choreo_type typ in 
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (var_node ^ edge ^ c, node_name)

(** [dot_pattern patn] creates the dot code for patterns [patn]
        
        - [patn] is a pattern.
        - Variants of patterns include Default, Var, Pair, LocPatt, Left, and Right.
        - For variant [LocPatt], it calls helper function [dot_local_pattern] to generate the dot code for the local pattern.
        - For variants [Pair], [Left], and [Right], it calls helper function [dot_pattern] to generate the dot code for the pattern.
        - Connects the pattern node to it's children.
        - Returns: A tuple of strings where the first element is the dot code for the pattern [patn] and the 
        second element is the node name of the pattern [patn].
*)
and dot_pattern patn = 
  let node_name = generate_node_name () in 
  match patn with 
  | Default -> Printf.sprintf "%s [label=\"Default\"];\n" node_name, node_name
  | Var (VarId id) -> Printf.sprintf "%s [label=\"%s\"];\n" node_name id, node_name
  | Pair (patn1, patn2) ->
      let (c1, n1) = dot_pattern patn1 in
      let (c2, n2) = dot_pattern patn2 in
      let pair_node = Printf.sprintf "%s [label=\"Pair\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | LocPatt (LocId loc, lp) -> 
      let locid_node = Printf.sprintf "%s [label=\"%s\"];\n" node_name loc in 
      let (c, n) = dot_local_pattern lp in 
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (locid_node ^ edge ^ c, node_name)
  | Left patn ->
      let (c, n) = dot_pattern patn in
      let left_node = Printf.sprintf "%s [label=\"Left\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (left_node ^ edge ^ c, node_name)
  | Right patn ->
      let (c, n) = dot_pattern patn in
      let right_node = Printf.sprintf "%s [label=\"Right\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (right_node ^ edge ^ c, node_name)

(** [dot_local_pattern patn] creates the dot code for local patterns [patn] 
    
    - [patn] is a local pattern.
    - Variants of local patterns include Default, Val, Var, Pair, Left, and Right.
    - For variants [Pair], [Left], and [Right], it calls helper function [dot_local_pattern] to generate the dot code for the local pattern.
    - Connects the local pattern node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local pattern [patn] and the 
      second element is the node name of the local pattern [patn].
*)
and dot_local_pattern (patn : local_pattern) : string * string =
  let node_name = generate_node_name () in
  match patn with
  | Default -> Printf.sprintf "%s [label=\"Default\"];\n" node_name, node_name
  | Val ((`Int _ | `String _ | `Bool _) as v) ->
      Printf.sprintf "%s [label=\"%s\"];\n" node_name (string_of_value v), node_name
  | Var (VarId id) -> Printf.sprintf "%s [label=\"%s\"];\n" node_name id, node_name
  | Pair (patn1, patn2) ->
      let (c1, n1) = dot_local_pattern patn1 in
      let (c2, n2) = dot_local_pattern patn2 in
      let pair_node = Printf.sprintf "%s [label=\"Pair\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Left patn ->
      let (c, n) = dot_local_pattern patn in
      let left_node = Printf.sprintf "%s [label=\"Left\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (left_node ^ edge ^ c, node_name)
  | Right patn ->
      let (c, n) = dot_local_pattern patn in
      let right_node = Printf.sprintf "%s [label=\"Right\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (right_node ^ edge ^ c, node_name)

(** [dot_choreo_type typ] creates the dot code for choreo types [typ] 
    
    - [typ] is a choreo type.
    - Variants of choreo types include TUnit, TLoc, TSend, TProd, and TSum.
    - For Variant [TLoc], it calls helper function [dot_local_type] to generate the dot code for the local type.
    - For variants [TSend], [TProd], and [TSum], it calls helper function [dot_choreo_type] to generate the dot code for the choreo type.
    - Connects the choreo type node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the choreo type [typ] and the 
      second element is the node name of the choreo type [typ].
*)
and dot_choreo_type typ = 
  let node_name = generate_node_name () in 
  match typ with 
  | TUnit -> Printf.sprintf "%s [label=\"()\"];\n" node_name, node_name
  | TLoc (LocId id, typ) ->
      let locid_node = Printf.sprintf "%s [label=\"%s\"];\n" node_name id in
      let (c, n) = dot_local_type typ in 
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (locid_node ^ edge ^ c, node_name)
  | TSend (typ1, typ2) ->
      let (c1, n1) = dot_choreo_type typ1 in 
      let (c2, n2) = dot_choreo_type typ2 in 
      let send_node = Printf.sprintf "%s [label=\"Send\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (send_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | TProd (typ1, typ2) ->
      let (c1, n1) = dot_choreo_type typ1 in
      let (c2, n2) = dot_choreo_type typ2 in
      let prod_node = Printf.sprintf "%s [label=\"Product\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (prod_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | TSum (typ1, typ2) ->
      let (c1, n1) = dot_choreo_type typ1 in
      let (c2, n2) = dot_choreo_type typ2 in
      let sum_node = Printf.sprintf "%s [label=\"Sum\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (sum_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
 
(** [dot_local_type typ] creates the dot code for local types [typ] 
    
    - [typ] is a local type.
    - Variants of local types include TUnit, TInt, TString, TBool, TProd, and TSum.
    - For variants [TProd] and [TSum], it calls helper function [dot_local_type] to generate the dot code for the local type.
    - Connects the local type node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local type [typ] and the 
      second element is the node name of the local type [typ].
*)
and dot_local_type (typ : local_type) : string * string =
  let node_name = generate_node_name () in
  match typ with
  | TUnit -> Printf.sprintf "%s [label=\"()\"];\n" node_name, node_name
  | TInt -> Printf.sprintf "%s [label=\"Int\"];\n" node_name, node_name
  | TString -> Printf.sprintf "%s [label=\"String\"];\n" node_name, node_name
  | TBool -> Printf.sprintf "%s [label=\"Bool\"];\n" node_name, node_name
  | TProd (typ1, typ2) ->
      let (c1, n1) = dot_local_type typ1 in
      let (c2, n2) = dot_local_type typ2 in
      let prod_node = Printf.sprintf "%s [label=\"Product\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (prod_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | TSum (typ1, typ2) ->
      let (c1, n1) = dot_local_type typ1 in
      let (c2, n2) = dot_local_type typ2 in
      let sum_node = Printf.sprintf "%s [label=\"Sum\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (sum_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)

(** [dot_choreo_expr chor_expr] creates the dot code for choreo expressions [chor_expr] 
    
    - [chor_expr] is a choreo expression.
    - Variants of choreo expressions include Unit, Var, LocExpr, Send, Sync, If, Let, FunDef, FunApp, Pair, Fst, Snd, Left, Right, and Match.
    - Calls helper functions [dot_local_expr], [dot_choreo_expr], [dot_stmts], [dot_choreo_cases], [dot_choreo_case], [dot_pattern], and [dot_choreo_expr] 
      to generate the dot code for the choreo expression.
    - Connects the choreo expression node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the choreo expression [chor_expr] and the 
      second element is the node name of the choreo expression [chor_expr].
*)
and dot_choreo_expr chor_expr = 
  let node_name = generate_node_name () in 
  match chor_expr with 
  | Unit -> Printf.sprintf "%s [label=\"()\"];\n" node_name, node_name
  | Var (VarId id) ->
      Printf.sprintf "%s [label=\"%s\"];\n" node_name id, node_name
  | LocExpr (LocId id, le) ->
      let locid_node = Printf.sprintf "%s [label=\"%s\"];\n" node_name id in
      let (c, n) = dot_local_expr le in 
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (locid_node ^ edge ^ c, node_name)
  | Send (expr, LocId loc_id) ->
      let (c, n) = dot_choreo_expr expr in 
      let send_node = Printf.sprintf "%s [label=\"Send: %s\"];\n" node_name loc_id in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (send_node ^ edge ^ c, node_name)
  | Sync (LocId loc_id1, LabelId label, LocId loc_id2, expr) ->
      let (c, n) = dot_choreo_expr expr in
      let sync_node =
        Printf.sprintf "%s [label=\"Sync: %s[%s] -> %s\"];\n" node_name loc_id1 label loc_id2 in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (sync_node ^ edge ^ c, node_name)
  | If (cond, then_expr, else_expr) ->
      let (c1, n1) = dot_choreo_expr cond in 
      let (c2, n2) = dot_choreo_expr then_expr in 
      let (c3, n3) = dot_choreo_expr else_expr in 
      let if_node = Printf.sprintf "%s [label=\"If\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      let edge3 = Printf.sprintf "%s -> %s;\n" node_name n3 in
      (if_node ^ edge1 ^ edge2 ^ edge3 ^ c1 ^ c2 ^ c3, node_name)
  | Let (decl_block, expr) ->
      let (c1, n1) = dot_stmts decl_block in 
      let (c2, n2) = dot_choreo_expr expr in 
      let let_node = Printf.sprintf "%s [label=\"Let\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (let_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | FunDef (VarId var_id, expr) ->
      let (c, n) = dot_choreo_expr expr in 
      let fundef_node = Printf.sprintf "%s [label=\"FunDef: %s\"];\n" node_name var_id in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (fundef_node ^ edge ^ c, node_name)
  | FunApp (f, arg) ->
      let (c1, n1) = dot_choreo_expr f in 
      let (c2, n2) = dot_choreo_expr arg in 
      let funapp_node = Printf.sprintf "%s [label=\"FunApp\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (funapp_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Pair (e1, e2) ->
      let (c1, n1) = dot_choreo_expr e1 in
      let (c2, n2) = dot_choreo_expr e2 in
      let pair_node = Printf.sprintf "%s [label=\"Pair\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Fst e ->
      let (c, n) = dot_choreo_expr e in
      let fst_node = Printf.sprintf "%s [label=\"Fst\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (fst_node ^ edge ^ c, node_name)
  | Snd e ->
      let (c, n) = dot_choreo_expr e in
      let snd_node = Printf.sprintf "%s [label=\"Snd\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (snd_node ^ edge ^ c, node_name)
  | Left e ->
      let (c, n) = dot_choreo_expr e in
      let left_node = Printf.sprintf "%s [label=\"Left\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (left_node ^ edge ^ c, node_name)
  | Right e ->
      let (c, n) = dot_choreo_expr e in
      let right_node = Printf.sprintf "%s [label=\"Right\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (right_node ^ edge ^ c, node_name)
  | Match (e, cases) ->
      let (c1, n1) = dot_choreo_expr e in
      let match_node = Printf.sprintf "%s [label=\"Match\"];\n" node_name in
      let (c2, n2) = dot_choreo_cases cases in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (match_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)

(** [dot_choreo_cases cases] calls [dot_choreo_case] on each choreo case in [cases] and creates a tuple.
        
        - [cases] is a list of choreo cases.
        - Returns: Tuples of strings where the first element is the dot code for the choreo cases in [cases] and the second element is the 
        node name of the choreo cases in [cases].
*)
and dot_choreo_cases cases =
  let case_nodes, case_node_names = List.split (List.map (dot_choreo_case) cases) in
  (String.concat "" case_nodes, String.concat " " case_node_names)

(** [dot_choreo_case (patn, expr)] creates the dot code for a choreo case [(patn, expr)] 
    
    - [(patn, expr)] is a tuple of a choreo pattern and a choreo expression.
    - Calls [dot_pattern] on the choreo pattern and [dot_choreo_expr] on the choreo expression.
    - Connects the [dot_choreo_case] node to it's children, the choreo pattern and choreo expression nodes.
    - Returns: A tuple of strings where the first element is the dot code for the choreo case [(patn, expr)] 
      and the second element is the node name of the choreo case [(patn, expr)].
*)
and dot_choreo_case (patn, expr) = 
  let node_name = generate_node_name () in 
  let (c1, patn_name) = dot_pattern patn in 
  let (c2, expr_name) = dot_choreo_expr expr in 
  let case_node = Printf.sprintf "%s [label=\"Case\"];\n" node_name in
  let edge1 = Printf.sprintf "%s -> %s;\n" node_name patn_name in
  let edge2 = Printf.sprintf "%s -> %s;\n" node_name expr_name in
  (case_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)

(** [dot_local_expr loc_expr] creates the dot code for local expressions [loc_expr] 
    
    - [loc_expr] is a local expression.
    - Variants of local expressions include Unit, Val, Var, BinOp, Let, Pair, Fst, Snd, Left, Right, and Match.
    - Calls helper functions [dot_bin_op], [dot_local_cases], [dot_local_case], [dot_local_pattern], and [dot_local_expr] 
      to generate the dot code for the local expression.
    - Connects the local expression node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local expression [loc_expr] and the 
      second element is the node name of the local expression [loc_expr].
*)
and dot_local_expr (loc_expr : local_expr) : string * string =
  let node_name = generate_node_name () in
  match loc_expr with
  | Unit -> Printf.sprintf "%s [label=\"()\"];\n" node_name, node_name
  | Val ((`Int _ | `String _ | `Bool _) as v) ->
      Printf.sprintf "%s [label=\"%s\"];\n" node_name (string_of_value v), node_name
  | Var (VarId id) ->
      Printf.sprintf "%s [label=\"%s\"];\n" node_name id, node_name
  | BinOp (e1, op, e2) ->
      let (c1, n1) = dot_local_expr e1 in
      let (c2, n2) = dot_local_expr e2 in
      let bin_op_node = Printf.sprintf "%s [label=\"BinOp\"];\n" node_name in
      let (c3, n3) = dot_bin_op op in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      let edge3 = Printf.sprintf "%s -> %s;\n" node_name n3 in
      (bin_op_node ^ edge1 ^ edge2 ^ edge3 ^ c1 ^ c2 ^ c3, node_name)
  | Let (VarId id, e1, e2) ->
      let (c1, n1) = dot_local_expr e1 in
      let (c2, n2) = dot_local_expr e2 in
      let let_node = Printf.sprintf "%s [label=\"Let: %s\"];\n" node_name id in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (let_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Pair (e1, e2) ->
      let (c1, n1) = dot_local_expr e1 in
      let (c2, n2) = dot_local_expr e2 in
      let pair_node = Printf.sprintf "%s [label=\"Pair\"];\n" node_name in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)
  | Fst e ->
      let (c, n) = dot_local_expr e in
      let fst_node = Printf.sprintf "%s [label=\"Fst\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (fst_node ^ edge ^ c, node_name)
  | Snd e ->
      let (c, n) = dot_local_expr e in
      let snd_node = Printf.sprintf "%s [label=\"Snd\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (snd_node ^ edge ^ c, node_name)
  | Left e ->
      let (c, n) = dot_local_expr e in
      let left_node = Printf.sprintf "%s [label=\"Left\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (left_node ^ edge ^ c, node_name)
  | Right e ->
      let (c, n) = dot_local_expr e in
      let right_node = Printf.sprintf "%s [label=\"Right\"];\n" node_name in
      let edge = Printf.sprintf "%s -> %s;\n" node_name n in
      (right_node ^ edge ^ c, node_name)
  | Match (e, cases) ->
      let (c1, n1) = dot_local_expr e in
      let match_node = Printf.sprintf "%s [label=\"Match\"];\n" node_name in
      let (c2, n2) = dot_local_cases cases in
      let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
      let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
      (match_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)

(** [dot_local_cases cases] calls [dot_local_case] on each local case in [cases] and creates a tuple. 
    
    - [cases] is a list of local cases.
    - Returns: Tuples of strings where the first element is the dot code for the local cases in [cases] and the second element is the 
      node name of the local cases in [cases].
*)
and dot_local_cases cases =
  let case_nodes, case_node_names = List.split (List.map (dot_local_case) cases) in
  (String.concat "" case_nodes, String.concat " " case_node_names)
(* Because of type matching error / List.iter is a type unit
List.append also seems to be wrong *)

(** [dot_local_case (patn, expr)] creates the dot code for a local case [(patn, expr)] 
    
    - [(patn, expr)] is a tuple of a local pattern and a local expression.
    - Calls [dot_local_pattern] on the local pattern and [dot_local_expr] on the local expression.
    - Connects the [dot_local_case] node to it's children, the local pattern and local expression nodes.
    - Returns: A tuple of strings where the first element is the dot code for the local case [(patn, expr)] 
      and the second element is the node name of the local case [(patn, expr)].
*)
and dot_local_case (patn, expr) =
  let node_name = generate_node_name () in
  let (c1, patn_name) = dot_local_pattern patn in
  let (c2, expr_name) = dot_local_expr expr in
  let case_node = Printf.sprintf "%s [label=\"Case\"];\n" node_name in
  let edge1 = Printf.sprintf "%s -> %s;\n" node_name patn_name in
  let edge2 = Printf.sprintf "%s -> %s;\n" node_name expr_name in
  (case_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name)

(** [dot_bin_op op] creates the dot code for the binary operator [op] 

    - [op] is a binary operator that is either Plus, Minus, Times, Div, And, Or, Eq, Neq, Lt, Leq, Gt, or Geq.
    - Each label for the binary operator is a string representation of the operator.
    - Returns: A tuple of strings where the first element is the dot code for the binary operator [op] and the 
      second element is the node name of the binary operator [op].
*)
and dot_bin_op (op : bin_op) : string * string =
  let node_name = generate_node_name () in
  match op with
  | Plus -> Printf.sprintf "%s [label=\"+\"];\n" node_name, node_name
  | Minus -> Printf.sprintf "%s [label=\"-\"];\n" node_name, node_name
  | Times -> Printf.sprintf "%s [label=\"*\"];\n" node_name, node_name
  | Div -> Printf.sprintf "%s [label=\"/\"];\n" node_name, node_name
  | And -> Printf.sprintf "%s [label=\"&&\"];\n" node_name, node_name
  | Or -> Printf.sprintf "%s [label=\"||\"];\n" node_name, node_name
  | Eq -> Printf.sprintf "%s [label=\"=\"];\n" node_name, node_name
  | Neq -> Printf.sprintf "%s [label=\"!=\"];\n" node_name, node_name
  | Lt -> Printf.sprintf "%s [label=\"<\"];\n" node_name, node_name
  | Leq -> Printf.sprintf "%s [label=\"<=\"];\n" node_name, node_name
  | Gt -> Printf.sprintf "%s [label=\">\"];\n" node_name, node_name
  | Geq -> Printf.sprintf "%s [label=\">=\"];\n" node_name, node_name

(** [string_of_value v] returns a string representation of the value [v] 
    
        - [v] is a value that is either an integer, string, or boolean.
        - Returns: A string representation of the value [v].
*)
and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> s
  | `Bool b -> string_of_bool b

