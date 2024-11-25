module Choreo = Ast_core.Choreo.M
module LocSet = Set.Make (String)

let rec extract_pattern : 'a Choreo.pattern -> LocSet.t = function
  | Default _ | Var _ -> LocSet.empty
  | Pair (p1, p2, _) -> LocSet.union (extract_pattern p1) (extract_pattern p2)
  | LocPat (LocId (id, _), _, _) -> LocSet.singleton id
  | Left (p, _) | Right (p, _) -> extract_pattern p
;;

let rec extract_type : 'a Choreo.typ -> LocSet.t = function
  | TUnit _ -> LocSet.empty
  | TLoc (LocId (id, _), _, _) -> LocSet.singleton id
  | TVar (Typ_Id (id, _), _) -> LocSet.singleton id
  | TMap (t1, t2, _) | TProd (t1, t2, _) | TSum (t1, t2, _) ->
    LocSet.union (extract_type t1) (extract_type t2)
;;

let[@specialise] rec extract_stmt_block (stmts : 'a Choreo.stmt_block) =
  List.fold_left (fun acc stmt -> LocSet.union acc (extract_stmt stmt)) LocSet.empty stmts

and extract_stmt : 'a Choreo.stmt -> LocSet.t = function
  | Decl (p, t, _) -> LocSet.union (extract_pattern p) (extract_type t)
  | Assign (ps, e, _) ->
    LocSet.union
      (List.fold_left (fun acc p -> LocSet.union acc (extract_pattern p)) LocSet.empty ps)
      (extract_expr e)
  | TypeDecl (_, t, _) -> extract_type t

and extract_expr : 'a Choreo.expr -> LocSet.t = function
  | Unit _ | Var _ -> LocSet.empty
  | LocExpr (LocId (id, _), _, _) -> LocSet.singleton id
  | Send (LocId (id1, _), e, LocId (id2, _), _) ->
    LocSet.add id2 (LocSet.add id1 (extract_expr e))
  | Sync (LocId (id1, _), _, LocId (id2, _), e, _) ->
    LocSet.add id2 (LocSet.add id1 (extract_expr e))
  | If (e1, e2, e3, _) ->
    LocSet.union (extract_expr e1) (LocSet.union (extract_expr e2) (extract_expr e3))
  | Let (stmts, e, _) -> LocSet.union (extract_stmt_block stmts) (extract_expr e)
  | FunDef (ps, e, _) ->
    LocSet.union
      (List.fold_left (fun acc p -> LocSet.union acc (extract_pattern p)) LocSet.empty ps)
      (extract_expr e)
  | FunApp (e1, e2, _) | Pair (e1, e2, _) ->
    LocSet.union (extract_expr e1) (extract_expr e2)
  | Fst (e, _) | Snd (e, _) | Left (e, _) | Right (e, _) -> extract_expr e
  | Match (e, cases, _) ->
    List.fold_left
      (fun acc (p, e) ->
        LocSet.union acc (LocSet.union (extract_pattern p) (extract_expr e)))
      (extract_expr e)
      cases
;;
