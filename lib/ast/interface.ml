open Choreo
open Yojson.Basic

let rec dump_choreo_ast prog = dump_program prog |> pretty_to_string

and dump_program (Prog (stmts, (filename, line))) =
  `Assoc [ ("File", `String filename); ("Lines", `Int line); ("decl_block", `List (List.map dump_stmt stmts)) ]

and dump_stmt = function
  | Decl (p, t, (_, line)) ->
      `Assoc
        [
          ( "Decl (Line: " ^ string_of_int line ^ ")", (* "-" ^ string_of_int eline ^ *) 
            `Assoc [ ("pattern", dump_pattern p); ("choreo_type", dump_choreo_type t) ]
          );
        ]
  | Assign (p, e, (_, line)) ->
      `Assoc
        [
          ( "Assign (Line: " ^ string_of_int line ^ ")",
            `Assoc [ ("pattern", dump_pattern p); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | TypeDecl (VarId (id, (_, id_line)), t, (_, line)) ->
      `Assoc
        [
          ( "TypeDecl (Line: " ^ string_of_int line ^ ")",
            `Assoc [ ("id (Line: " ^ string_of_int id_line ^ ")", `String id); ("choreo_type", dump_choreo_type t) ]
          );
        ]

and dump_choreo_expr = function
  | Unit (_, line) -> `String ("Unit (Line: " ^ string_of_int line ^ ")")
  | Var (VarId (id, _), (_, line)) ->
      `Assoc [ ("Var (Line: " ^ string_of_int line ^ ")", `String id) ]
  | Fst (e, (_, line)) ->
      `Assoc [ ("Fst (Line: " ^ string_of_int line ^ ")", dump_choreo_expr e) ]
  | Snd (e, (_, line)) ->
      `Assoc [ ("Snd (Line: " ^ string_of_int line ^ ")", dump_choreo_expr e) ]
  | Left (e, (_, line)) ->
      `Assoc [ ("Left (Line: " ^ string_of_int line ^ ")", dump_choreo_expr e) ]
  | Right (e, (_, line)) ->
      `Assoc [ ("Right (Line: " ^ string_of_int line ^ ")", dump_choreo_expr e) ]
  | LocExpr (LocId (loc, (_, loc_line)), e, (_, line)) ->
      `Assoc
        [
          ( "LocExpr (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [ ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_expr", dump_local_expr e) ]
          );
        ]
  | Send (e, LocId (loc, (_, loc_line)), (_, line)) ->
      `Assoc
        [
          ( "Send (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [ ("choreo_expr", dump_choreo_expr e); ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc) ]
          );
        ]
  | Sync (LocId (loc1, (_, loc1_line)), LabelId (label, (_, label_line)), LocId (loc2, (_, loc2_line)), e, (_, line)) ->
      `Assoc
        [
          ( "Sync (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("loc1 (Line: " ^ string_of_int loc1_line ^ ")", `String loc1);
                ("label (Line: " ^ string_of_int label_line ^ ")", `String label);
                ("loc2 (Line: " ^ string_of_int loc2_line ^ ")", `String loc2);
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | If (e1, e2, e3, (_, line)) ->
      `Assoc
        [
          ( "If Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("condition", dump_choreo_expr e1);
                ("then", dump_choreo_expr e2);
                ("else", dump_choreo_expr e3);
              ] );
        ]
  | Let (decl_block, e, (_, line)) ->
      `Assoc
        [
          ( "Let (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("decl_block", `List (List.map dump_stmt decl_block));
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | FunDef (VarId (id, (_, id_line)), e, (_, line)) ->
      `Assoc
        [
          ( "FunDef (Line: " ^ string_of_int line ^ ")",
            `Assoc [ ("id (Line: " ^ string_of_int id_line ^ ")", `String id); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | FunApp (e1, e2, (_, line)) ->
      `Assoc
        [
          ( "FunApp (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [ ("fun", dump_choreo_expr e1); ("arg", dump_choreo_expr e2) ] );
        ]
  | Pair (e1, e2, (_, line)) ->
      `Assoc [ ("Pair (Line: " ^ string_of_int line ^ ")", `List [ dump_choreo_expr e1; dump_choreo_expr e2 ]) ]
  | Match (e, cases, (_, line)) ->
      `Assoc
        [
          ( "Match (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("choreo_expr", dump_choreo_expr e);
                ("cases", `List (List.map dump_case cases));
              ] );
        ]

and dump_local_expr = function
  | Unit (_, line) -> `String ("Unit (Line: " ^ string_of_int line ^ ")")
  | Val ((`Int _ | `String _ | `Bool _ as v), _) ->
       (match v with
        | `Int (i, (_, line)) ->`Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String (string_of_int i) ) ]
        | `String (s, (_, line)) -> `Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String s ) ]
        | `Bool (b, (_, line)) -> `Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String (string_of_bool b) ) ])
  | Var (VarId (id, _), (_, line)) ->
      `Assoc [ ("Var (Line: " ^ string_of_int line ^ ")", `String id) ]
  | Fst (e, (_, line)) ->
      `Assoc [ ("Fst (Line: " ^ string_of_int line ^ ")", dump_local_expr e) ]
  | Snd (e, (_, line)) ->
      `Assoc [ ("Snd (Line: " ^ string_of_int line ^ ")", dump_local_expr e) ]
  | Left (e, (_, line)) ->
      `Assoc [ ("Left (Line: " ^ string_of_int line ^ ")", dump_local_expr e) ]
  | Right (e, (_, line)) ->
      `Assoc [ ("Right (Line: " ^ string_of_int line ^ ")", dump_local_expr e) ]
  | Pair (e1, e2, (_, line)) ->
      `Assoc [ ("Pair (Line: " ^ string_of_int line ^ ")", `List [ dump_local_expr e1; dump_local_expr e2 ]) ]
  | BinOp (e1, op, e2, (_, line)) ->
      `Assoc
        [
          ( "BinOp (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("choreo_e1", dump_local_expr e1);
                ("op", dump_bin_op op);
                ("choreo_e2", dump_local_expr e2);
              ] );
        ]
  | Let (VarId (id, (_, id_line)), e1, e2, (_, line)) ->
      `Assoc
        [
          ( "Let (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("id (Line: " ^ string_of_int id_line ^ ")", `String id);
                ("binding", dump_local_expr e1);
                ("body", dump_local_expr e2);
              ] );
        ]
  | Match (e, cases, (_, line)) ->
      `Assoc
        [
          ( "Match (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("local_expr", dump_local_expr e);
                ("cases", `List (List.map dump_local_case cases));
              ] );
        ]

and dump_case (p, e) =
  `Assoc [ ("pattern", dump_pattern p); ("choreo_expr", dump_choreo_expr e) ]

and dump_local_case (p, e) =
  `Assoc [ ("local_patt", dump_local_pattern p); ("local_expr", dump_local_expr e) ]

and dump_pattern = function
  | Default (_, line) -> `String ("Default (Line: " ^ string_of_int line ^ ")")
  | Var (VarId (id, _), (_, line)) ->
      `Assoc [ ("Var (Line: " ^ string_of_int line ^ ")", `String id) ]
  | Left (p, (_, line)) ->
      `Assoc [ ("Left (Line: " ^ string_of_int line ^ ")", dump_pattern p) ]
  | Right (p, (_, line)) ->
      `Assoc [ ("Right (Line: " ^ string_of_int line ^ ")", dump_pattern p) ]
  | Pair (p1, p2, (_, line)) ->
      `Assoc [ ("Pair (Line: " ^ string_of_int line ^ ")", `List [ dump_pattern p1; dump_pattern p2 ]) ]
  | LocPatt (LocId (loc, (_, loc_line)), p, (_,line)) ->
      `Assoc
        [
          ( "LocPatt (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [
                ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_patt", dump_local_pattern p);
              ] );
        ]

and dump_local_pattern = function
  | Default (_, line) -> `String ("Default (Line: " ^ string_of_int line ^ ")")
  | Val ((`Int _ | `String _ | `Bool _ as v), _)   ->
      (match v with
      | `Int (i, (_, line)) ->`Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String (string_of_int i) ) ]
      | `String (s, (_, line)) -> `Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String s ) ]
      | `Bool (b, (_, line)) -> `Assoc [ ("Val  (Line: " ^ string_of_int line ^ ")", `String (string_of_bool b) ) ])
  | Var (VarId (id, _), (_, line)) ->
      `Assoc [ ("Var (Line: " ^ string_of_int line ^ ")", `String id) ]
  | Left (p, (_, line)) ->
      `Assoc [ ("Left (Line: " ^ string_of_int line ^ ")", dump_local_pattern p) ]
  | Right (p, (_, line)) ->
      `Assoc [ ("Right (Line: " ^ string_of_int line ^ ")", dump_local_pattern p) ]
  | Pair (p1, p2, (_, line)) ->
      `Assoc
        [ ("Pair (Line: " ^ string_of_int line ^ ")", `List [ dump_local_pattern p1; dump_local_pattern p2 ]) ]

and dump_choreo_type = function
  | TUnit (_, line) -> `String ("TUnit (Line: " ^ string_of_int line ^ ")")
  | TLoc (LocId (loc, (_, loc_line)), t, (_, line)) ->
      `Assoc
        [
          ( "TLoc (Line: " ^ string_of_int line ^ ")",
            `Assoc
              [ ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_type", dump_local_type t) ]
          );
        ]
  | TSend (t1, t2, (_, line)) ->
      `Assoc [ ("TSend (Line: " ^ string_of_int line ^ ")", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TProd (t1, t2, (_, line)) ->
      `Assoc [ ("TProd (Line: " ^ string_of_int line ^ ")", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TSum (t1, t2, (_, line)) ->
      `Assoc [ ("TSum (Line: " ^ string_of_int line ^ ")", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]

and dump_local_type = function
  | TUnit (_, line) -> `String ("TUnit (Line: " ^ string_of_int line ^ ")")
  | TInt (_, line) -> `String ("TInt (Line: " ^ string_of_int line ^ ")")
  | TString (_, line) -> `String ("TString (Line: " ^ string_of_int line ^ ")")
  | TBool (_, line) -> `String ("TBool (Line: " ^ string_of_int line ^ ")")
  | TProd (t1, t2, (_, line)) ->
      `Assoc [ ("TProd (Line: " ^ string_of_int line ^ ")", `List [ dump_local_type t1; dump_local_type t2 ]) ]
  | TSum (t1, t2, (_, line)) ->
      `Assoc [ ("TSum (Line: " ^ string_of_int line ^ ")", `List [ dump_local_type t1; dump_local_type t2 ]) ]


(** [dump_bin_op op] returns a string representation of the binary operator [op] 
    including metainfo(see ast/local.ml).
    
    - For example:
      [dump_bin_op (Plus (_, 42))] returns ["Plus (Line: 42)"].
    
    This function is used primarily for debugging and logging purposes, to provide a clear,
    human-readable representation of binary operations within the AST (Abstract Syntax Tree).
*)

and dump_bin_op = function
  | Plus (_, line) -> `String ("Plus" ^ " (Line: " ^ string_of_int line ^ ")")
  | Minus (_, line) -> `String ("Minus" ^ " (Line: " ^ string_of_int line ^ ")")
  | Times (_, line) -> `String ("Times" ^ " (Line: " ^ string_of_int line ^ ")")
  | Div (_, line) -> `String ("Div" ^ " (Line: " ^ string_of_int line ^ ")")
  | And (_, line) -> `String ("And" ^ " (Line: " ^ string_of_int line ^ ")")
  | Or (_, line) -> `String ("Or" ^ " (Line: " ^ string_of_int line ^ ")")
  | Eq (_, line) -> `String ("Eq" ^ " (Line: " ^ string_of_int line ^ ")")
  | Neq (_, line) -> `String ("Neq" ^ " (Line: " ^ string_of_int line ^ ")")
  | Lt (_, line) -> `String ("Lt" ^ " (Line: " ^ string_of_int line ^ ")")
  | Leq (_, line) -> `String ("Leq" ^ " (Line: " ^ string_of_int line ^ ")")
  | Gt (_, line) -> `String ("Gt" ^ " (Line: " ^ string_of_int line ^ ")")
  | Geq (_, line) -> `String ("Geq" ^ " (Line: " ^ string_of_int line ^ ")")