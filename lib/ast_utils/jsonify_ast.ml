module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

(* ============================== Local ============================== *)
let jsonify_un_op = function
  | Local.Not _ -> `String "Not"
  | Local.Neg _ -> `String "Neg"

and jsonify_bin_op = function
  | Local.Plus _ -> `String "Plus"
  | Local.Minus _ -> `String "Minus"
  | Local.Times _ -> `String "Times"
  | Local.Div _ -> `String "Div"
  | Local.And _ -> `String "And"
  | Local.Or _ -> `String "Or"
  | Local.Eq _ -> `String "Eq"
  | Local.Neq _ -> `String "Neq"
  | Local.Lt _ -> `String "Lt"
  | Local.Leq _ -> `String "Leq"
  | Local.Gt _ -> `String "Gt"
  | Local.Geq _ -> `String "Geq"
;;

let rec jsonify_local_type = function
  | Local.TUnit _ -> `String "TUnit"
  | Local.TInt _ -> `String "TInt"
  | Local.TString _ -> `String "TString"
  | Local.TBool _ -> `String "TBool"
  | Local.TVar (TypId (id, _), _) -> `String id
  | Local.TProd (t1, t2, _) ->
    `Assoc [ "TProd", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]
  | Local.TSum (t1, t2, _) ->
    `Assoc [ "TSum", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]
;;

let rec jsonify_local_pattern = function
  | Local.Default _ -> `String "Default"
  | Local.Val (v, _) ->
    `Assoc
      [ ( "Val"
        , match v with
          | Int (i, _) -> `Int i
          | String (s, _) -> `String s
          | Bool (b, _) -> `Bool b )
      ]
  | Local.Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Local.Left (p, _) -> `Assoc [ "Left", jsonify_local_pattern p ]
  | Local.Right (p, _) -> `Assoc [ "Right", jsonify_local_pattern p ]
  | Local.Pair (p1, p2, _) ->
    `Assoc [ "Pair", `List [ jsonify_local_pattern p1; jsonify_local_pattern p2 ] ]
;;

let rec jsonify_local_expr = function
  | Local.Unit _ -> `String "Unit"
  | Local.Val (v, _) ->
    `Assoc
      [ ( "Val"
        , match v with
          | Int (i, _) -> `Int i
          | String (s, _) -> `String s
          | Bool (b, _) -> `Bool b )
      ]
  | Local.Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Local.Fst (e, _) -> `Assoc [ "Fst", jsonify_local_expr e ]
  | Local.Snd (e, _) -> `Assoc [ "Snd", jsonify_local_expr e ]
  | Local.Left (e, _) -> `Assoc [ "Left", jsonify_local_expr e ]
  | Local.Right (e, _) -> `Assoc [ "Right", jsonify_local_expr e ]
  | Local.Pair (e1, e2, _) ->
    `Assoc [ "Pair", `List [ jsonify_local_expr e1; jsonify_local_expr e2 ] ]
  | Local.UnOp (op, e, _) ->
    `Assoc [ "UnOp", `Assoc [ "op", jsonify_un_op op; "choreo_e", jsonify_local_expr e ] ]
  | Local.BinOp (e1, op, e2, _) ->
    `Assoc
      [ ( "BinOp"
        , `Assoc
            [ "choreo_e1", jsonify_local_expr e1
            ; "op", jsonify_bin_op op
            ; "choreo_e2", jsonify_local_expr e2
            ] )
      ]
  | Local.Let (VarId (id, _), t, e1, e2, _) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "id", `String id
            ; "local_type", jsonify_local_type t
            ; "binding", jsonify_local_expr e1
            ; "body", jsonify_local_expr e2
            ] )
      ]
  | Local.Match (e, cases, _) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "local_expr", jsonify_local_expr e
            ; ( "cases"
              , `List
                  (List.map
                     (fun (p, e) ->
                       `Assoc
                         [ "local_patt", jsonify_local_pattern p
                         ; "local_expr", jsonify_local_expr e
                         ])
                     cases) )
            ] )
      ]
;;

(* ============================== Choreo ============================== *)
let rec jsonify_choreo_type = function
  | Choreo.TUnit _ -> `String "TUnit"
  | Choreo.TLoc (LocId (loc, _), t, _) ->
    `Assoc [ "TLoc", `Assoc [ "loc", `String loc; "local_type", jsonify_local_type t ] ]
  | Choreo.TVar (Typ_Id (id, _), _) -> `String id
  | Choreo.TMap (t1, t2, _) ->
    `Assoc [ "TMap", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | Choreo.TProd (t1, t2, _) ->
    `Assoc [ "TProd", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | Choreo.TSum (t1, t2, _) ->
    `Assoc [ "TSum", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
;;

let rec jsonify_choreo_pattern = function
  | Choreo.Default _ -> `String "Default"
  | Choreo.Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Choreo.Left (p, _) -> `Assoc [ "Left", jsonify_choreo_pattern p ]
  | Choreo.Right (p, _) -> `Assoc [ "Right", jsonify_choreo_pattern p ]
  | Choreo.Pair (p1, p2, _) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_pattern p1; jsonify_choreo_pattern p2 ] ]
  | Choreo.LocPat (LocId (loc, _), p, _) ->
    `Assoc
      [ "LocPat", `Assoc [ "loc", `String loc; "local_patt", jsonify_local_pattern p ] ]
;;

let rec jsonify_choreo_stmt = function
  | Choreo.Decl (p, t, _) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "choreo_pattern", jsonify_choreo_pattern p
            ; "choreo_type", jsonify_choreo_type t
            ] )
      ]
  | Choreo.Assign (ps, e, _) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "choreo_patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | Choreo.TypeDecl (TypId (id, _), t, _) ->
    `Assoc
      [ "TypeDecl", `Assoc [ "id", `String id; "choreo_type", jsonify_choreo_type t ] ]

and jsonify_choreo_expr = function
  | Choreo.Unit _ -> `String "Unit"
  | Choreo.Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Choreo.Fst (e, _) -> `Assoc [ "Fst", jsonify_choreo_expr e ]
  | Choreo.Snd (e, _) -> `Assoc [ "Snd", jsonify_choreo_expr e ]
  | Choreo.Left (e, _) -> `Assoc [ "Left", jsonify_choreo_expr e ]
  | Choreo.Right (e, _) -> `Assoc [ "Right", jsonify_choreo_expr e ]
  | Choreo.LocExpr (LocId (loc, _), e, _) ->
    `Assoc
      [ "LocExpr", `Assoc [ "loc", `String loc; "local_expr", jsonify_local_expr e ] ]
  | Choreo.Send (LocId (loc1, _), e, LocId (loc2, _), _) ->
    `Assoc
      [ ( "Send"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "from", `String loc1
            ; "to", `String loc2
            ] )
      ]
  | Choreo.Sync (LocId (loc1, _), LabelId (label, _), LocId (loc2, _), e, _) ->
    `Assoc
      [ ( "Sync"
        , `Assoc
            [ "loc1", `String loc1
            ; "label", `String label
            ; "loc2", `String loc2
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | Choreo.If (e1, e2, e3, _) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_choreo_expr e1
            ; "then", jsonify_choreo_expr e2
            ; "else", jsonify_choreo_expr e3
            ] )
      ]
  | Choreo.Let (stmts, e, _) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_choreo_stmt stmts)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | Choreo.FunDef (ps, e, _) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | Choreo.FunApp (e1, e2, _) ->
    `Assoc
      [ "FunApp", `Assoc [ "fun", jsonify_choreo_expr e1; "arg", jsonify_choreo_expr e2 ]
      ]
  | Choreo.Pair (e1, e2, _) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_expr e1; jsonify_choreo_expr e2 ] ]
  | Choreo.Match (e, cases, _) ->
    let[@inline] jsonify_choreo_case (p, e) =
      `Assoc
        [ "choreo_pattern", jsonify_choreo_pattern p
        ; "choreo_expr", jsonify_choreo_expr e
        ]
    in
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "cases", `List (List.map jsonify_choreo_case cases)
            ] )
      ]
;;

let[@inline] jsonify_choreo_stmt_block (stmts : 'a Choreo.stmt_block) =
  `List (List.map jsonify_choreo_stmt stmts)
;;

(* ============================== Net ============================== *)

let rec jsonify_net_type = function
  | Net.TUnit _ -> `String "TUnit"
  | Net.TLoc (t, _) -> `Assoc [ "TLoc", jsonify_local_type t ]
  | Net.TMap (t1, t2, _) ->
    `Assoc [ "TMap", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | Net.TProd (t1, t2, _) ->
    `Assoc [ "TProd", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | Net.TSum (t1, t2, _) ->
    `Assoc [ "TSum", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
;;

let rec jsonify_net_stmt = function
  | Net.Decl (p, t, _) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "net_pattern", jsonify_local_pattern p; "net_type", jsonify_net_type t ] )
      ]
  | Net.Assign (ps, e, _) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "net_pattern", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | Net.TypeDecl (TypId (id, _), t, _) ->
    `Assoc [ "TypeDecl", `Assoc [ "id", `String id; "net_type", jsonify_net_type t ] ]

and jsonify_net_expr = function
  | Net.Unit _ -> `String "Unit"
  | Net.Var (VarId (id, _), _) -> `Assoc [ "Var", `String id ]
  | Net.Ret (e, _) -> `Assoc [ "Ret", jsonify_local_expr e ]
  | Net.If (e1, e2, e3, _) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_net_expr e1
            ; "then", jsonify_net_expr e2
            ; "else", jsonify_net_expr e3
            ] )
      ]
  | Net.Let (stmts, e, _) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_net_stmt stmts)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | Net.Send (e, LocId (loc, _), _) ->
    `Assoc [ "Send", `Assoc [ "net_expr", jsonify_net_expr e; "to", `String loc ] ]
  | Net.Recv (LocId (loc, _), _) -> `Assoc [ "Recv", `String loc ]
  | Net.ChooseFor (LabelId (label, _), LocId (loc, _), e, _) ->
    `Assoc
      [ ( "ChooseFor"
        , `Assoc
            [ "label", `String label; "loc", `String loc; "net_expr", jsonify_net_expr e ]
        )
      ]
  | Net.AllowChoice (LocId (loc, _), choices, _) ->
    `Assoc
      [ ( "AllowChoice"
        , `Assoc
            [ "loc", `String loc
            ; ( "choices"
              , `List
                  (List.map
                     (fun (Local.LabelId (label, _), e) ->
                       `Assoc [ "label", `String label; "net_expr", jsonify_net_expr e ])
                     choices) )
            ] )
      ]
  | Net.FunDef (ps, e, _) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | Net.FunApp (e1, e2, _) ->
    `Assoc [ "FunApp", `Assoc [ "fun", jsonify_net_expr e1; "arg", jsonify_net_expr e2 ] ]
  | Net.Pair (e1, e2, _) ->
    `Assoc [ "Pair", `List [ jsonify_net_expr e1; jsonify_net_expr e2 ] ]
  | Net.Fst (e, _) -> `Assoc [ "Fst", jsonify_net_expr e ]
  | Net.Snd (e, _) -> `Assoc [ "Snd", jsonify_net_expr e ]
  | Net.Left (e, _) -> `Assoc [ "Left", jsonify_net_expr e ]
  | Net.Right (e, _) -> `Assoc [ "Right", jsonify_net_expr e ]
  | Net.Match (e, cases, _) ->
    let jsonify_net_case (p, e) =
      `Assoc [ "net_pattern", jsonify_local_pattern p; "net_expr", jsonify_net_expr e ]
    in
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "net_expr", jsonify_net_expr e
            ; "cases", `List (List.map jsonify_net_case cases)
            ] )
      ]
;;

let jsonify_net_stmt_block (stmts : 'a Net.stmt_block) =
  `List (List.map jsonify_net_stmt stmts)
;;
