(* ============================== Local ============================== *)

let rec jsonify_local_expr (e : Ast.Local.expr) =
  match e with
  | Unit _ -> `String "Unit"
  | Val ((`Int _ | `String _ | `Bool _ as v), _) ->
    (match v with
     | `Int (i, _) ->`Assoc [ "Val", `Int i ]
     | `String (s, _) -> `Assoc [ "Val", `String s ]
     | `Bool (b, _) -> `Assoc [ "Val", `Bool b ])
  | Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Fst (e, _) -> `Assoc [ "Fst", jsonify_local_expr e ]
  | Snd (e, _) -> `Assoc [ "Snd", jsonify_local_expr e ]
  | Left (e, _) -> `Assoc [ "Left", jsonify_local_expr e ]
  | Right (e, _) -> `Assoc [ "Right", jsonify_local_expr e ]
  | Pair (e1, e2, _) ->
    `Assoc [ "Pair", `List [ jsonify_local_expr e1; jsonify_local_expr e2 ] ]
  | UnOp (op, e, _) ->
    `Assoc [ "UnOp", `Assoc [ "op", jsonify_un_op op; "choreo_e", jsonify_local_expr e ] ]
  | BinOp (e1, op, e2, _) ->
    `Assoc
      [ ( "BinOp"
        , `Assoc
            [ "choreo_e1", jsonify_local_expr e1
            ; "op", jsonify_bin_op op
            ; "choreo_e2", jsonify_local_expr e2
            ] )
      ]
  | Let (VarId (id, _), e1, e2, _) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "id", `String id
            ; "binding", jsonify_local_expr e1
            ; "body", jsonify_local_expr e2
            ] )
      ]
  | Match (e, cases, _) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "local_expr", jsonify_local_expr e
            ; "cases", `List (List.map jsonify_local_case cases)
            ] )
      ]

and jsonify_local_case (p, e) =
  `Assoc [ "local_patt", jsonify_local_pattern p; "local_expr", jsonify_local_expr e ]

and jsonify_local_pattern = function
  | Default _ -> `String "Default"
  | Val ((`Int _ | `String _ | `Bool _ as v), _) ->
    (match v with
     | `Int (i, _) ->`Assoc [ "Val", `Int i ]
     | `String (s, _) -> `Assoc [ "Val", `String s ]
     | `Bool (b, _) -> `Assoc [ "Val", `Bool b ])
  | Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Left (p, _) -> `Assoc [ "Left", jsonify_local_pattern p ]
  | Right (p, _) -> `Assoc [ "Right", jsonify_local_pattern p ]
  | Pair (p1, p2, _) ->
    `Assoc [ "Pair", `List [ jsonify_local_pattern p1; jsonify_local_pattern p2 ] ]

and jsonify_local_type (t : Ast.Local.typ) =
  match t with
  | TUnit _ -> `String "TUnit"
  | TInt _ -> `String "TInt"
  | TString _ -> `String "TString"
  | TBool _ -> `String "TBool"
  | TProd (t1, t2, _) ->
    `Assoc [ "TProd", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]
  | TSum (t1, t2, _) ->
    `Assoc [ "TSum", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]

and jsonify_un_op = function
  | Not _ -> `String "Not"
  | Neg _ -> `String "Neg"

and jsonify_bin_op = function
  | Plus _ -> `String "Plus"
  | Minus _ -> `String "Minus"
  | Times _ -> `String "Times"
  | Div _ -> `String "Div"
  | And _ -> `String "And"
  | Or _ -> `String "Or"
  | Eq _ -> `String "Eq"
  | Neq _ -> `String "Neq"
  | Lt _ -> `String "Lt"
  | Leq _ -> `String "Leq"
  | Gt _ -> `String "Gt"
  | Geq _ -> `String "Geq"
;;

(* ============================== Choreo ============================== *)

let rec jsonify_choreo_stmt_block (stmts : Ast.Choreo.stmt_block) =
  `List (List.map jsonify_choreo_stmt stmts)

and jsonify_choreo_stmt = function
  | Decl (p, t, _) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "choreo_pattern", jsonify_choreo_pattern p
            ; "choreo_type", jsonify_choreo_type t
            ] )
      ]
  | Assign (ps, e, _) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "choreo_patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | TypeDecl (TypId (id, _), t, _) ->
    `Assoc
      [ "TypeDecl", `Assoc [ "id", `String id; "choreo_type", jsonify_choreo_type t ] ]

and jsonify_choreo_expr = function
  | Unit _ -> `String "Unit"
  | Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Fst (e, _) -> `Assoc [ "Fst", jsonify_choreo_expr e ]
  | Snd (e, _) -> `Assoc [ "Snd", jsonify_choreo_expr e ]
  | Left (e, _) -> `Assoc [ "Left", jsonify_choreo_expr e ]
  | Right (e, _) -> `Assoc [ "Right", jsonify_choreo_expr e ]
  | LocExpr (LocId (loc, _), e, _) ->
    `Assoc
      [ "LocExpr", `Assoc [ "loc", `String loc; "local_expr", jsonify_local_expr e ] ]
  | Send (LocId (loc1, _), e, LocId (loc2, _), _) ->
    `Assoc
      [ ( "Send"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "from", `String loc1
            ; "to", `String loc2
            ] )
      ]
  | Sync (LocId (loc1, _), LabelId (label, _), LocId (loc2, _), e, _) ->
    `Assoc
      [ ( "Sync"
        , `Assoc
            [ "loc1", `String loc1
            ; "label", `String label
            ; "loc2", `String loc2
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | If (e1, e2, e3, _) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_choreo_expr e1
            ; "then", jsonify_choreo_expr e2
            ; "else", jsonify_choreo_expr e3
            ] )
      ]
  | Let (stmts, e, _) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_choreo_stmt stmts)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | FunDef (ps, e, _) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | FunApp (e1, e2, _) ->
    `Assoc
      [ "FunApp", `Assoc [ "fun", jsonify_choreo_expr e1; "arg", jsonify_choreo_expr e2 ]
      ]
  | Pair (e1, e2, _) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_expr e1; jsonify_choreo_expr e2 ] ]
  | Match (e, cases, _) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "cases", `List (List.map jsonify_choreo_case cases)
            ] )
      ]

and jsonify_choreo_case (p, e) =
  `Assoc
    [ "choreo_pattern", jsonify_choreo_pattern p; "choreo_expr", jsonify_choreo_expr e ]

and jsonify_choreo_pattern = function
  | Default _ -> `String "Default"
  | Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Left (p, _) -> `Assoc [ "Left", jsonify_choreo_pattern p ]
  | Right (p, _) -> `Assoc [ "Right", jsonify_choreo_pattern p ]
  | Pair (p1, p2, _) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_pattern p1; jsonify_choreo_pattern p2 ] ]
  | LocPatt (LocId (loc, _), p, _) ->
    `Assoc
      [ "LocPatt", `Assoc [ "loc", `String loc; "local_patt", jsonify_local_pattern p ] ]

and jsonify_choreo_type = function
  | TUnit _ -> `String "TUnit"
  | TLoc (LocId (loc, _), t, _) ->
    `Assoc [ "TLoc", `Assoc [ "loc", `String loc; "local_type", jsonify_local_type t ] ]
  | TMap (t1, t2, _) ->
    `Assoc [ "TMap", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TProd (t1, t2, _) ->
    `Assoc [ "TProd", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TSum (t1, t2, _) ->
    `Assoc [ "TSum", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TAlias (TypId (id, _), t, _) ->
    `Assoc [ "TAlias", `Assoc [ "id", `String id; "choreo_type", jsonify_choreo_type t ] ]
;;

(* ============================== Net ============================== *)

let rec jsonify_net_stmt_block (stmts : Ast.Net.stmt_block) =
  `List (List.map jsonify_net_stmt stmts)

and jsonify_net_stmt = function
  | Decl (p, t) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "net_pattern", jsonify_local_pattern p; "net_type", jsonify_net_type t ] )
      ]
  | Assign (ps, e) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "net_pattern", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | TypeDecl (TypId (id, _), t) ->
    `Assoc [ "TypeDecl", `Assoc [ "id", `String id; "net_type", jsonify_net_type t ] ]

and jsonify_net_case (p, e) =
  `Assoc [ "net_pattern", jsonify_local_pattern p; "net_expr", jsonify_net_expr e ]

and jsonify_net_expr = function
  | Unit -> `String "Unit"
  | Var (VarId (id, _)) -> `Assoc [ "Var", `String id ]
  | Ret e -> `Assoc [ "Ret", jsonify_local_expr e ]
  | If (e1, e2, e3) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_net_expr e1
            ; "then", jsonify_net_expr e2
            ; "else", jsonify_net_expr e3
            ] )
      ]
  | Let (stmts, e) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_net_stmt stmts)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | Send (e, LocId (loc, _)) ->
    `Assoc [ "Send", `Assoc [ "net_expr", jsonify_net_expr e; "to", `String loc ] ]
  | Recv (LocId (loc, _)) -> `Assoc [ "Recv", `String loc ]
  | ChooseFor (LabelId (label, _), LocId (loc, _), e) ->
    `Assoc
      [ ( "ChooseFor"
        , `Assoc
            [ "label", `String label; "loc", `String loc; "net_expr", jsonify_net_expr e ]
        )
      ]
  | AllowChoice (LocId (loc, _), choices) ->
    `Assoc
      [ ( "AllowChoice"
        , `Assoc
            [ "loc", `String loc
            ; ( "choices"
              , `List
                  (List.map
                     (fun (Ast.Local.LabelId (label, _), e) ->
                       `Assoc [ "label", `String label; "net_expr", jsonify_net_expr e ])
                     choices) )
            ] )
      ]
  | FunDef (ps, e) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | FunApp (e1, e2) ->
    `Assoc [ "FunApp", `Assoc [ "fun", jsonify_net_expr e1; "arg", jsonify_net_expr e2 ] ]
  | Pair (e1, e2) -> `Assoc [ "Pair", `List [ jsonify_net_expr e1; jsonify_net_expr e2 ] ]
  | Fst e -> `Assoc [ "Fst", jsonify_net_expr e ]
  | Snd e -> `Assoc [ "Snd", jsonify_net_expr e ]
  | Left e -> `Assoc [ "Left", jsonify_net_expr e ]
  | Right e -> `Assoc [ "Right", jsonify_net_expr e ]
  | Match (e, cases) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "net_expr", jsonify_net_expr e
            ; "cases", `List (List.map jsonify_net_case cases)
            ] )
      ]

and jsonify_net_type = function
  | TUnit -> `String "TUnit"
  | TLoc t -> `Assoc [ "TLoc", jsonify_local_type t ]
  | TMap (t1, t2) -> `Assoc [ "TMap", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | TProd (t1, t2) ->
    `Assoc [ "TProd", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | TSum (t1, t2) -> `Assoc [ "TSum", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
;;
