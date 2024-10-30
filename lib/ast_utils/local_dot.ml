module Local = Parsing.Parsed_ast.Local

let string_of_pos = Parsing.Parsed_ast.Pos_info.string_of_pos
let spf = Printf.sprintf
let node_counter = ref 0

(** [generate_node_name] generates a node name for the dot code

    - This function reads the current node counter, increments it, and concatentates it with the node_id to create a node name.
    - Returns: A string that represents a node name in the dot code. *)
let generate_node_name () =
  let node_id = !node_counter in
  node_counter := !node_counter + 1;
  "n" ^ string_of_int node_id
;;

(** [string_of_value v] returns a string representation of the value [v]

    - [v] is a value that is either an integer, string, or boolean.
    - Returns: A string representation of the value [v]. *)
let string_of_value (v : Local.value) =
  match v with
  | Int (i, _) -> string_of_int i
  | String (s, _) -> s
  | Bool (b, _) -> string_of_bool b
;;

(** [dot_bin_op op] creates the dot code for the binary operator [op]

    - [op] is a binary operator that is either Plus, Minus, Times, Div, And, Or, Eq, Neq, Lt, Leq, Gt, or Geq.
    - Each label for the binary operator is a string representation of the operator.
    - Returns: A tuple of strings where the first element is the dot code for the binary operator [op] and the
      second element is the node name of the binary operator [op]. *)
let dot_bin_op (op : Local.bin_op) : string * string =
  let node_name = generate_node_name () in
  match op with
  | Plus pos -> spf "%s [label=\"+ %s\"];\n" node_name (string_of_pos pos), node_name
  | Minus pos -> spf "%s [label=\"- %s\"];\n" node_name (string_of_pos pos), node_name
  | Times pos -> spf "%s [label=\"* %s\"];\n" node_name (string_of_pos pos), node_name
  | Div pos -> spf "%s [label=\"/ %s\"];\n" node_name (string_of_pos pos), node_name
  | And pos -> spf "%s [label=\"&& %s\"];\n" node_name (string_of_pos pos), node_name
  | Or pos -> spf "%s [label=\"|| %s\"];\n" node_name (string_of_pos pos), node_name
  | Eq pos -> spf "%s [label=\"= %s\"];\n" node_name (string_of_pos pos), node_name
  | Neq pos -> spf "%s [label=\"!= %s\"];\n" node_name (string_of_pos pos), node_name
  | Lt pos -> spf "%s [label=\"< %s\"];\n" node_name (string_of_pos pos), node_name
  | Leq pos -> spf "%s [label=\"<= %s\"];\n" node_name (string_of_pos pos), node_name
  | Gt pos -> spf "%s [label=\"> %s\"];\n" node_name (string_of_pos pos), node_name
  | Geq pos -> spf "%s [label=\">= %s\"];\n" node_name (string_of_pos pos), node_name
;;

let dot_un_op (op : Local.un_op) : string * string =
  let node_name = generate_node_name () in
  match op with
  | Not pos -> spf "%s [label=\"! %s\"];\n" node_name (string_of_pos pos), node_name
  | Neg pos -> spf "%s [label=\"¬ %s\"];\n" node_name (string_of_pos pos), node_name
;;

(** [dot_local_type typ] creates the dot code for local types [typ]

    - [typ] is a local type.
    - Variants of local types include TUnit, TInt, TString, TBool, TProd, and TSum.
    - For variants [TProd] and [TSum], it calls helper function [dot_local_type] to generate the dot code for the local type.
    - Connects the local type node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local type [typ] and the
      second element is the node name of the local type [typ]. *)
let rec dot_local_type (typ : Local.typ) : string * string =
  let node_name = generate_node_name () in
  match typ with
  | TUnit pos -> spf "%s [label=\"() %s\"];\n" node_name (string_of_pos pos), node_name
  | TInt pos -> spf "%s [label=\"Int %s\"];\n" node_name (string_of_pos pos), node_name
  | TString pos ->
    spf "%s [label=\"String %s\"];\n" node_name (string_of_pos pos), node_name
  | TVar (TypId (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | TBool pos -> spf "%s [label=\"Bool %s\"];\n" node_name (string_of_pos pos), node_name
  | TProd (typ1, typ2, pos) ->
    let c1, n1 = dot_local_type typ1 in
    let c2, n2 = dot_local_type typ2 in
    let prod_node = spf "%s [label=\"Product %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    prod_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | TSum (typ1, typ2, pos) ->
    let c1, n1 = dot_local_type typ1 in
    let c2, n2 = dot_local_type typ2 in
    let sum_node = spf "%s [label=\"Sum %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    sum_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
;;

(* node name format: n + node_counter *)

(** [dot_local_pattern pat] creates the dot code for local patterns [pat]

    - [pat] is a local pattern.
    - Variants of local patterns include Default, Val, Var, Pair, Left, and Right.
    - For variants [Pair], [Left], and [Right], it calls helper function [dot_local_pattern] to generate the dot code for the local pattern.
    - Connects the local pattern node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local pattern [pat] and the
      second element is the node name of the local pattern [pat]. *)
let rec dot_local_pattern (pat : Local.pattern) : string * string =
  let node_name = generate_node_name () in
  match pat with
  | Default pos ->
    spf "%s [label=\"Default %s\"];\n" node_name (string_of_pos pos), node_name
  | Val (v, _) ->
    (match v with
     | Int (i, pos) ->
       ( spf "%s [label=\"%s %s\"];\n" node_name (string_of_int i) (string_of_pos pos)
       , node_name )
     | String (s, pos) ->
       spf "%s [label=\"%s %s\"];\n" node_name s (string_of_pos pos), node_name
     | Bool (b, pos) ->
       ( spf "%s [label=\"%s %s\"];\n" node_name (string_of_bool b) (string_of_pos pos)
       , node_name ))
  | Var (VarId (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | Pair (pat1, pat2, pos) ->
    let c1, n1 = dot_local_pattern pat1 in
    let c2, n2 = dot_local_pattern pat2 in
    let pair_node = spf "%s [label=\"Pair %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Left (pat, pos) ->
    let c, n = dot_local_pattern pat in
    let left_node = spf "%s [label=\"Left %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (pat, pos) ->
    let c, n = dot_local_pattern pat in
    let right_node = spf "%s [label=\"Right %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name
;;

(** [dot_local_expr loc_expr] creates the dot code for local expressions [loc_expr]

    - [loc_expr] is a local expression.
    - Variants of local expressions include Unit, Val, Var, BinOp, Let, Pair, Fst, Snd, Left, Right, and Match.
    - Calls helper functions [dot_bin_op], [dot_local_cases], [dot_local_case], [dot_local_pattern], and [dot_local_expr]
      to generate the dot code for the local expression.
    - Connects the local expression node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local expression [loc_expr] and the
      second element is the node name of the local expression [loc_expr]. *)
let rec dot_local_expr (loc_expr : Local.expr) : string * string =
  let node_name = generate_node_name () in
  match loc_expr with
  | Unit pos -> spf "%s [label=\"() %s\"];\n" node_name (string_of_pos pos), node_name
  | Val (v, _) ->
    (match v with
     | Int (i, pos) ->
       ( spf "%s [label=\"%s %s\"];\n" node_name (string_of_int i) (string_of_pos pos)
       , node_name )
     | String (s, pos) ->
       spf "%s [label=\"%s %s\"];\n" node_name s (string_of_pos pos), node_name
     | Bool (b, pos) ->
       ( spf "%s [label=\"%s %s\"];\n" node_name (string_of_bool b) (string_of_pos pos)
       , node_name ))
  | Var (VarId (id, _), pos) ->
    spf "%s [label=\"%s %s\"];\n" node_name id (string_of_pos pos), node_name
  | UnOp (op, e, pos) ->
    let c1, n1 = dot_local_expr e in
    let un_op_node = spf "%s [label=\"UnOp %s\"];\n" node_name (string_of_pos pos) in
    let c2, n2 = dot_un_op op in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    un_op_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | BinOp (e1, op, e2, pos) ->
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let bin_op_node = spf "%s [label=\"BinOp %s\"];\n" node_name (string_of_pos pos) in
    let c3, n3 = dot_bin_op op in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    let edge3 = spf "%s -> %s;\n" node_name n3 in
    bin_op_node ^ edge1 ^ edge2 ^ edge3 ^ c1 ^ c2 ^ c3, node_name
  | Let (VarId (id, _), _, e1, e2, pos) ->
    (*the Let is changed to take in the additional type!!!!!!!!!!!!!!!!!!!!!!!!!*)
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let let_node = spf "%s [label=\"Let: %s %s\"];\n" node_name id (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    let_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Pair (e1, e2, pos) ->
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let pair_node = spf "%s [label=\"Pair %s\"];\n" node_name (string_of_pos pos) in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Fst (e, pos) ->
    let c, n = dot_local_expr e in
    let fst_node = spf "%s [label=\"Fst %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    fst_node ^ edge ^ c, node_name
  | Snd (e, pos) ->
    let c, n = dot_local_expr e in
    let snd_node = spf "%s [label=\"Snd %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    snd_node ^ edge ^ c, node_name
  | Left (e, pos) ->
    let c, n = dot_local_expr e in
    let left_node = spf "%s [label=\"Left %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (e, pos) ->
    let c, n = dot_local_expr e in
    let right_node = spf "%s [label=\"Right %s\"];\n" node_name (string_of_pos pos) in
    let edge = spf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name
  | Match (e, cases, pos) ->
    (* [dot_local_case (pat, expr)] creates the dot code for a local case [(pat, expr)]

       - [(pat, expr)] is a tuple of a local pattern and a local expression.
       - Calls [dot_local_pattern] on the local pattern and [dot_local_expr] on the local expression.
       - Connects the [dot_local_case] node to it's children, the local pattern and local expression nodes.
       - Returns: A tuple of strings where the first element is the dot code for the local case [(pat, expr)]
         and the second element is the node name of the local case [(pat, expr)]. *)
    let[@inline] dot_local_case (pat, expr) =
      let node_name = generate_node_name () in
      let c1, pat_name = dot_local_pattern pat in
      let c2, expr_name = dot_local_expr expr in
      let case_node = spf "%s [label=\"Case\"];\n" node_name in
      let edge1 = spf "%s -> %s;\n" node_name pat_name in
      let edge2 = spf "%s -> %s;\n" node_name expr_name in
      case_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
    in
    (* [dot_local_cases cases] calls [dot_local_case] on each local case in [cases] and creates a tuple.

       - [cases] is a list of local cases.
       - Returns: Tuples of strings where the first element is the dot code for the local cases in [cases] and the second element is the
         node name of the local cases in [cases]. *)
    let[@inline] dot_local_cases cases =
      let case_nodes, case_node_names = List.split (List.map dot_local_case cases) in
      String.concat "" case_nodes, String.concat " " case_node_names
    in
    (* Because of type matching error / List.iter is a type unit
       List.append also seems to be wrong *)
    let c1, n1 = dot_local_expr e in
    let match_node = spf "%s [label=\"Match %s\"];\n" node_name (string_of_pos pos) in
    let c2, n2 = dot_local_cases cases in
    let edge1 = spf "%s -> %s;\n" node_name n1 in
    let edge2 = spf "%s -> %s;\n" node_name n2 in
    match_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
;;
