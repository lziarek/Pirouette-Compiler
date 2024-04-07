type value =
  [ `Int of int
  | `String of string
  | `Bool of bool ]

type loc_id = LocId of string
type var_id = VarId of string
type sync_label = LabelId of string

type bin_op =
  | Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

type local_type =
  | TUnit
  | TInt
  | TString
  | TBool
  | TProd of local_type * local_type
  | TSum of local_type * local_type

type local_pattern =
  | Default
  | Val of value
  | Var of var_id
  | Pair of local_pattern * local_pattern
  | Left of local_pattern
  | Right of local_pattern

type local_expr =
  | Unit
  | Val of value
  | Var of var_id
  | BinOp of local_expr * bin_op * local_expr
  | Let of var_id * local_expr * local_expr
  | Pair of local_expr * local_expr
  | Fst of local_expr
  | Snd of local_expr
  | Left of local_expr
  | Right of local_expr
  | Match of local_expr * (local_pattern * local_expr) list

let node_counter = ref 0

let generate_node_name (node_counter) =
  let node_id = !node_counter in
  node_counter := !node_counter + 1;
  "n" ^ string_of_int node_id
(* node name format: n + node_counter *)

let rec dot_local_expr file_oc loc_expr =
  let node_name = generate_node_name () in
  match loc_expr with
  | Unit -> Printf.fprintf file_oc "%s [label=\"Unit\"];\n" node_name
  | Val ((`Int _ | `String _ | `Bool _) as v) ->
      Printf.fprintf file_oc "%s [label=\"%s\"];\n" node_name (string_of_value v)
  | Var (VarId id) ->
      Printf.fprintf file_oc "%s [label=\"%s\"];\n" node_name id
  | BinOp (e1, op, e2) ->
      Printf.fprintf file_oc "%s [label=\"BinOp\"];\n" node_name;
      dot_local_expr file_oc e1;
      dot_bin_op file_oc op;
      dot_local_expr file_oc e2
  | Let (VarId id, e1, e2) ->
      Printf.fprintf file_oc "%s [label=\"Let: %s\"];\n" node_name id;
      dot_local_expr file_oc e1;
      dot_local_expr file_oc e2
  | Pair (e1, e2) ->
      Printf.fprintf file_oc "%s [label=\"Pair\"];\n" node_name;
      dot_local_expr file_oc e1;
      dot_local_expr file_oc e2
  | Fst e ->
      Printf.fprintf file_oc "%s [label=\"Fst\"];\n" node_name;
      dot_local_expr file_oc e
  | Snd e ->
      Printf.fprintf file_oc "%s [label=\"Snd\"];\n" node_name;
      dot_local_expr file_oc e
  | Left e ->
      Printf.fprintf file_oc "%s [label=\"Left\"];\n" node_name;
      dot_local_expr file_oc e
  | Right e ->
      Printf.fprintf file_oc "%s [label=\"Right\"];\n" node_name;
      dot_local_expr file_oc e
  | Match (e, cases) ->
      Printf.fprintf file_oc "%s [label=\"Match\"];\n" node_name;
      dot_local_expr file_oc e;
      dot_local_cases file_oc cases

and dot_local_pattern file_oc patn =
  let node_name = generate_node_name () in
  match patn with
  | Default -> Printf.fprintf file_oc "%s [label=\"Default\"];\n" node_name
  | Val v -> Printf.fprintf file_oc "%s [label=\"%s\"];\n" node_name (string_of_value v)
  | Var (VarId id) -> Printf.fprintf file_oc "%s [label=\"%s\"];\n" node_name id
  | Pair (patn1, patn2) ->
      Printf.fprintf file_oc "%s [label=\"Pair\"];\n" node_name;
      dot_local_pattern file_oc patn1;
      dot_local_pattern file_oc patn2
  | Left patn ->
      Printf.fprintf file_oc "%s [label=\"Left\"];\n" node_name;
      dot_local_pattern file_oc patn
  | Right patn ->
      Printf.fprintf file_oc "%s [label=\"Right\"];\n" node_name;
      dot_local_pattern file_oc patn

and dot_local_type file_oc typ =
  let node_name = generate_node_name () in
  match typ with
  | TUnit -> Printf.fprintf file_oc "%s [label=\"Unit\"];\n" node_name
  | TInt -> Printf.fprintf file_oc "%s [label=\"Int\"];\n" node_name
  | TString -> Printf.fprintf file_oc "%s [label=\"String\"];\n" node_name
  | TBool -> Printf.fprintf file_oc "%s [label=\"Bool\"];\n" node_name
  | TProd (typ1, typ2) ->
      Printf.fprintf file_oc "%s [label=\"Product\"];\n" node_name;
      dot_local_type file_oc typ1;
      dot_local_type file_oc typ2
  | TSum (typ1, typ2) ->
      Printf.fprintf file_oc "%s [label=\"Sum\"];\n" node_name;
      dot_local_type file_oc typ1;
      dot_local_type file_oc typ2

and dot_bin_op file_oc op =
  let node_name = generate_node_name () in
  match op with
  | Plus -> Printf.fprintf file_oc "%s [label=\"+\"];\n" node_name
  | Minus -> Printf.fprintf file_oc "%s [label=\"-\"];\n" node_name
  | Times -> Printf.fprintf file_oc "%s [label=\"*\"];\n" node_name
  | Div -> Printf.fprintf file_oc "%s [label=\"/\"];\n" node_name
  | And -> Printf.fprintf file_oc "%s [label=\"&&\"];\n" node_name
  | Or -> Printf.fprintf file_oc "%s [label=\"||\"];\n" node_name
  | Eq -> Printf.fprintf file_oc "%s [label=\"=\"];\n" node_name
  | Neq -> Printf.fprintf file_oc "%s [label=\"!=\"];\n" node_name
  | Lt -> Printf.fprintf file_oc "%s [label=\"<\"];\n" node_name
  | Leq -> Printf.fprintf file_oc "%s [label=\"<=\"];\n" node_name
  | Gt -> Printf.fprintf file_oc "%s [label=\">\"];\n" node_name
  | Geq -> Printf.fprintf file_oc "%s [label=\">=\"];\n" node_name

and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> s
  | `Bool b -> string_of_bool b

and dot_local_cases file_oc cases = List.iter (dot_local_case file_oc) cases

and dot_local_case file_oc (patn, expr) = 
  let node_name = generate_node_name () in
  Printf.fprintf file_oc "%s [label=\"Case\"];\n" node_name;
  dot_local_pattern file_oc patn;
  dot_local_expr file_oc expr

let generate_dot_code filename program =
  let file_oc = open_out filename in
  Printf.fprintf file_oc "digraph G {\n";
  dot_local_expr file_oc program;
  Printf.fprintf file_oc "}\n";
  close_out file_oc

let () =
  if Array.length Sys.argv < 3 then (
    print_endline "Usage: <executable> <sourcefile> <destfile>";
    exit 1)
  else
    let in_filename = Sys.argv.(1) in
    let file_ic = open_in in_filename in
    let out_filename = Sys.argv.(2) ^ ".dot" in
    let file_write (s : string) =
      Printf.fprintf file_oc "%s\n" s
    in
    let lexbuf = Lexing.from_channel file_ic in
    let program = parse_program lexbuf in
    generate_dot_code out_filename program;
    close_in file_ic