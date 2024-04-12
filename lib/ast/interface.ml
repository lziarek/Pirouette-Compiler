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
  | TypeDecl (VarId (id, (_, id_line)), t) ->
      `Assoc
        [
          ( "TypeDecl",
            `Assoc [ ("id (Line: " ^ string_of_int id_line ^ ")", `String id); ("choreo_type", dump_choreo_type t) ]
          );
        ]

and dump_choreo_expr = function
  | Unit -> `String "Unit"
  | Var (VarId (id, (_, id_line))) ->
      `Assoc [ ("Var (Line: " ^ string_of_int id_line ^ ")", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", dump_choreo_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", dump_choreo_expr e) ]
  | Left e ->
      `Assoc [ ("Left", dump_choreo_expr e) ]
  | Right e ->
      `Assoc [ ("Right", dump_choreo_expr e) ]
  | LocExpr (LocId (loc, (_, loc_line)), e) ->
      `Assoc
        [
          ( "LocExpr",
            `Assoc
              [ ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_expr", dump_local_expr e) ]
          );
        ]
  | Send (e, LocId (loc, (_, loc_line))) ->
      `Assoc
        [
          ( "Send",
            `Assoc
              [ ("choreo_expr", dump_choreo_expr e); ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc) ]
          );
        ]
  | Sync (LocId (loc1, (_, loc1_line)), LabelId (label, _), LocId (loc2, (_, loc2_line)), e) ->
      `Assoc
        [
          ( "Sync",
            `Assoc
              [
                ("loc1 (Line: " ^ string_of_int loc1_line ^ ")", `String loc1);
                ("label", `String label);
                ("loc2 (Line: " ^ string_of_int loc2_line ^ ")", `String loc2);
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | If (e1, e2, e3) ->
      `Assoc
        [
          ( "If",
            `Assoc
              [
                ("condition", dump_choreo_expr e1);
                ("then", dump_choreo_expr e2);
                ("else", dump_choreo_expr e3);
              ] );
        ]
  | Let (decl_block, e) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("decl_block", `List (List.map dump_stmt decl_block));
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | FunDef (VarId (id, (_, id_line)), e) ->
      `Assoc
        [
          ( "FunDef",
            `Assoc [ ("id (Line: " ^ string_of_int id_line ^ ")", `String id); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | FunApp (e1, e2) ->
      `Assoc
        [
          ( "FunApp",
            `Assoc
              [ ("fun", dump_choreo_expr e1); ("arg", dump_choreo_expr e2) ] );
        ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ dump_choreo_expr e1; dump_choreo_expr e2 ]) ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
            `Assoc
              [
                ("choreo_expr", dump_choreo_expr e);
                ("cases", `List (List.map dump_case cases));
              ] );
        ]

and dump_local_expr = function
  | Unit -> `String "Unit"
  | Val (`Int _ | `String _ | `Bool _ as v) ->
      `Assoc [ ("Val", v) ]
  | Var (VarId (id, (_, id_line))) ->
      `Assoc [ ("Var (Line: " ^ string_of_int id_line ^ ")", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", dump_local_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", dump_local_expr e) ]
  | Left e ->
      `Assoc [ ("Left", dump_local_expr e) ]
  | Right e ->
      `Assoc [ ("Right", dump_local_expr e) ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ dump_local_expr e1; dump_local_expr e2 ]) ]
  | BinOp (e1, op, e2) ->
      `Assoc
        [
          ( "BinOp",
            `Assoc
              [
                ("choreo_e1", dump_local_expr e1);
                ("op", dump_bin_op op);
                ("choreo_e2", dump_local_expr e2);
              ] );
        ]
  | Let (VarId (id, (_, id_line)), e1, e2) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("id (Line: " ^ string_of_int id_line ^ ")", `String id);
                ("binding", dump_local_expr e1);
                ("body", dump_local_expr e2);
              ] );
        ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
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
  | Default -> `String "Default"
  | Var (VarId (id, (_, id_line))) ->
      `Assoc [ ("Var (Line: " ^ string_of_int id_line ^ ")", `String id) ]
  | Left p ->
      `Assoc [ ("Left", dump_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", dump_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc [ ("Pair", `List [ dump_pattern p1; dump_pattern p2 ]) ]
  | LocPatt (LocId (loc, (_, loc_line)), p) ->
      `Assoc
        [
          ( "LocPatt",
            `Assoc
              [
                ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_patt", dump_local_pattern p);
              ] );
        ]

and dump_local_pattern = function
  | Default -> `String "Default"
  | Val (`Int _ | `String _ | `Bool _ as v)  ->
      `Assoc [ ("Val", v) ]
  | Var (VarId (id, (_, id_line))) ->
      `Assoc [ ("Var (Line: " ^ string_of_int id_line ^ ")", `String id) ]
  | Left p ->
      `Assoc [ ("Left", dump_local_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", dump_local_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc
        [ ("Pair", `List [ dump_local_pattern p1; dump_local_pattern p2 ]) ]

and dump_choreo_type = function
  | TUnit -> `String "TUnit"
  | TLoc (LocId (loc, (_, loc_line)), t) ->
      `Assoc
        [
          ( "TLoc",
            `Assoc
              [ ("loc (Line: " ^ string_of_int loc_line ^ ")", `String loc); ("local_type", dump_local_type t) ]
          );
        ]
  | TSend (t1, t2) ->
      `Assoc [ ("TSend", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]

and dump_local_type = function
  | TUnit -> `String "TUnit"
  | TInt -> `String "TInt"
  | TString -> `String "TString"
  | TBool -> `String "TBool"
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ dump_local_type t1; dump_local_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ dump_local_type t1; dump_local_type t2 ]) ]

and dump_bin_op = function
  | Plus -> `String "Plus"
  | Minus -> `String "Minus"
  | Times -> `String "Times"
  | Div -> `String "Div"
  | And -> `String "And"
  | Or -> `String "Or"
  | Eq -> `String "Eq"
  | Neq -> `String "Neq"
  | Lt -> `String "Lt"
  | Leq -> `String "Leq"
  | Gt -> `String "Gt"
  | Geq -> `String "Geq"