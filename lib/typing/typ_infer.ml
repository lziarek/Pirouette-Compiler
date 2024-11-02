(* ============================== Types ============================== *)
module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M

(*type inference*)
type errmsg = string
type typvar = string
type ftv = (typvar, errmsg) result
type local_subst = (typvar * ftv Local.typ) list
type choreo_subst = (typvar * ftv Choreo.typ) list

(*context: list of pair of variable name and its binding*)
type local_ctx = (string * ftv Local.typ) list
type choreo_ctx = (string * ftv Choreo.typ) list
type global_ctx = (string * ftv Local.typ) list

(*free type variables*)
let (_m : ftv) = Ok "dummy info"

let rec unify t1 t2 : local_subst =
  match t1, t2 with
  | Local.TInt _, Local.TInt _
  | Local.TBool _, Local.TBool _
  | Local.TString _, Local.TString _
  | Local.TUnit _, Local.TUnit _ -> []
  | Local.TVar (Local.TypId (var_name, _), _), t
  | t, Local.TVar (Local.TypId (var_name, _), _) ->
    if t = Local.TVar (Local.TypId (var_name, _m), _m)
    then []
    else if occurs_in var_name t
    then failwith "Occurs check failed"
    else [ var_name, t ]
  | Local.TProd (t1a, t1b, _), Local.TProd (t2a, t2b, _)
  | Local.TSum (t1a, t1b, _), Local.TSum (t2a, t2b, _) ->
    let s1 = unify t1a t2a in
    let s2 = unify (apply_subst_typ s1 t1b) (apply_subst_typ s1 t2b) in
    s1 @ s2
  | _ -> failwith "Unification failed"

(*occurs check: ensure t1 does not occur in t2*)
and occurs_in var_name t2 =
  match t2 with
  | Local.TInt _ | Local.TBool _ | Local.TString _ | Local.TUnit _ -> false
  | Local.TVar (Local.TypId (var_name', _), _) -> var_name = var_name'
  | Local.TProd (t2a, t2b, _) | Local.TSum (t2a, t2b, _) ->
    occurs_in var_name t2a || occurs_in var_name t2b

(*traverse the substitution list `s`, apply all occurences of subst to `t`*)
and apply_subst_typ s t =
  match t with
  | (Local.TInt _ | Local.TBool _ | Local.TString _ | Local.TUnit _) as t' -> t'
  | Local.TVar (Local.TypId (var_name, _), _) ->
    (try List.assoc var_name s with
     | Not_found -> failwith "Type variable not found when applying substitution")
  | Local.TProd (t1, t2, _) -> Local.TProd (apply_subst_typ s t1, apply_subst_typ s t2, _m)
  | Local.TSum (t1, t2, _) -> Local.TSum (apply_subst_typ s t1, apply_subst_typ s t2, _m)

(*apply substitution to context*)
and apply_subst_ctx subst ctx =
  List.map (fun (var_name, var_type) -> var_name, apply_subst_typ subst var_type) ctx

(*generate unique free type variables*)
and gen_ftv =
  let counter = ref 0 in
  fun () ->
    let v = !counter in
    counter := !counter + 1;
    "T" ^ string_of_int v
;;

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*return Result type*)
and ctx_lookup ctx var_name =
  try Ok (List.assoc var_name ctx) with
  | Not_found -> Error "Variable not found"

(*extract loc_id's local context from glocal context*)
and extract_local_ctx global_ctx loc_id =
  List.fold_right
    (fun (loc, var_name, typ) acc -> if loc = loc_id then (var_name, typ) :: acc else acc)
    global_ctx
    []
;;

(* ============================== Local ============================== *)

let rec infer_local_expr local_ctx = function
  | Local.Unit _ -> [], Local.TUnit _m
  | Local.Val (v, _) -> [], typeof_Val v
  | Local.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup local_ctx var_name with
     | Ok t -> [], t
     | _ -> failwith "Variable not found when inferring expression")
  | Local.UnOp (op, e, _) ->
    (match op with
     | Local.Neg _ ->
       let s1, t = infer_local_expr local_ctx e in
       let t' = apply_subst_typ s1 t in
       let s2 = unify t' (Local.TInt _m) in
       s1 @ s2, Local.TInt _m
     | Local.Not _ ->
       let s1, t = infer_local_expr local_ctx e in
       let t' = apply_subst_typ s1 t in
       let s2 = unify t' (Local.TBool _m) in
       s1 @ s2, Local.TBool _m)
  | Local.BinOp (e1, op, e2, _) ->
    (match op with
     | Local.Plus _ | Local.Minus _ | Local.Times _ | Local.Div _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt _m) in
       let s4 = unify t2' (Local.TInt _m) in
       s1 @ s2 @ s3 @ s4, Local.TInt _m
     | Local.Eq _ | Local.Neq _ | Local.Lt _ | Local.Leq _ | Local.Gt _ | Local.Geq _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt _m) in
       let s4 = unify t2' (Local.TInt _m) in
       s1 @ s2 @ s3 @ s4, Local.TBool _m
     | Local.And _ | Local.Or _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TBool _m) in
       let s4 = unify t2' (Local.TBool _m) in
       s1 @ s2 @ s3 @ s4, Local.TBool _m)
  | Local.Let (Local.VarId (var_name, _), local_type, e1, e2, _) ->
    let s1, t1 = infer_local_expr local_ctx e1 in
    if t1 = local_type
    then (
      let s2, t2 = infer_local_expr (add_binding local_ctx var_name local_type) e2 in
      s1 @ s2, t2)
    else failwith "Type annotation and actual type mismatch"
  | Local.Pair (e1, e2, _) ->
    let s1, t1 = infer_local_expr local_ctx e1 in
    let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
    s1 @ s2, Local.TProd (apply_subst_typ s2 t1, apply_subst_typ s2 t2, _m)
  | Local.Fst (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    (match t with
     | Local.TProd (t1, _, _) -> s1, t1
     | _ -> failwith "Fst Type error")
  | Local.Snd (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    (match t with
     | Local.TProd (_, t2, _) -> s1, t2
     | _ -> failwith "Snd Type error")
  | Local.Left (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    (match t with
     | Local.TSum (t1, _, _) -> s1, Local.TSum (t1, apply_subst_typ s1 t, _m)
     | _ -> failwith "Left Type error")
  | Local.Right (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    (match t with
     | Local.TSum (_, t2, _) -> s1, Local.TSum (apply_subst_typ s1 t, t2, _m)
     | _ -> failwith "Right Type error")
  | Local.Match (e, cases, _) ->
    (*infer expr*)
    let s1, t_match = infer_local_expr local_ctx e in
    let t_match' = apply_subst_typ s1 t_match in
    (*infer cases*)
    let patt_ls, expr_ls = List.split cases in
    (*infer patterns to get list of each subst, each types, each ctx*)
    let ls = List.map (fun patt -> infer_local_pattern local_ctx patt) patt_ls in
    let s2, t_ls, ctx_ls =
      List.fold_right
        (fun (s, t, ctx) (s_acc, t_acc, ctx_acc) -> s @ s_acc, t :: t_acc, ctx :: ctx_acc)
        ls
        ([], [], [])
    in
    (* for each type, apply subst to rewrite the types',then unify with type of t_match'*)
    let t_ls' = List.map (fun t -> apply_subst_typ s2 t) t_ls in
    let s3 = List.fold_right (fun t acc -> unify t t_match' @ acc) t_ls' [] in
    (*for each sub exprs, infer it with contexts from each patterns*)
    (*apply, unify, return typ of expr*)
    (*also check if type mismatch during list of patterns and exprs*)
    (*infer each sub expr by each ctx@local_ctx, get a list of substs with list of types*)
    let s4, typ_ls =
      List.fold_right
        (fun (expr, ctx) (s_acc, typ_acc) ->
          let s, typ = infer_local_expr (apply_subst_ctx s2 ctx @ local_ctx) expr in
          s @ s_acc, typ :: typ_acc)
        (List.combine expr_ls ctx_ls)
        ([], [])
    in
    let typ_ls' = List.map (fun t -> apply_subst_typ s4 t) typ_ls in
    let s5 = List.fold_left (fun acc t -> unify t (List.hd typ_ls') @ acc) [] typ_ls' in
    s1 @ s2 @ s3 @ s4 @ s5, apply_subst_typ s5 (List.hd typ_ls')
(*unify each type with expected type*)

and typeof_Val = function
  | Int _ -> TInt _m
  | Bool _ -> TBool _m
  | String _ -> TString _m

and infer_local_pattern local_ctx = function
  | Local.Default _ -> [], Local.TUnit _m, []
  | Local.Val (v, _) -> [], typeof_Val v, []
  | Local.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup local_ctx var_name with
     | Ok t -> [], t, [ var_name, t ]
     | _ -> failwith "Variable not found when inferring pattern")
  | Local.Pair (p1, p2, _) ->
    let s1, t1, ctx1 = infer_local_pattern local_ctx p1 in
    let s2, t2, ctx2 = infer_local_pattern (apply_subst_ctx s1 local_ctx) p2 in
    s1 @ s2, Local.TProd (apply_subst_typ s2 t1, apply_subst_typ s2 t2, _m), ctx1 @ ctx2
  | Local.Left (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (t, Local.TVar (Local.TypId (gen_ftv (), _m), _m), _m), ctx
  | Local.Right (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (Local.TVar (Local.TypId (gen_ftv (), _m), _m), t, _m), ctx
;;

(* ============================== Choreo ============================== *)

let rec infer_choreo_stmt choreo_ctx global_ctx stmt : choreo_subst * ftv Choreo.typ =
  match stmt with
  | Choreo.Decl (pattn, choreo_typ, _) -> failwith "Not implemented"
  | Choreo.Assign (pattn_ls, expr, _) -> infer_choreo_expr choreo_ctx global_ctx expr
  | Choreo.TypeDecl (TypId (id, _), choreo_typ, _) -> failwith "Not implemented"

and infer_choreo_expr choreo_ctx global_ctx _expr = failwith "Not implemented"

and infer_choreo_stmt_block stmts =
  let choreo_ctx = [] in
  let subst, t =
    List.fold_left
      (fun (s_acc, g_acc) stmt -> infer_choreo_stmt s_acc g_acc stmt)
      (choreo_ctx, Choreo.TUnit _m)
      stmts
  in
  if subst == [] then t else failwith "Substitution not empty"
;;

(* let rec check_local_expr ctx expected_typ = function
   | Local.Unit _ -> expected_typ = Local.TUnit m
   | Val (v, _) -> expected_typ = typeof_Val v
   | Var (VarId (var_name, _), _) ->
   (match ctx_lookup ctx var_name with
   | Ok t -> expected_typ = t
   | _ -> false)
   | UnOp (op, e, _) ->
   (match op with
   | Neg _ -> check_local_expr ctx (TInt m) e && expected_typ = TInt m
   | Not _ -> check_local_expr ctx (TBool m) e && expected_typ = TBool m)
   | BinOp (e1, op, e2, _) ->
   (match op with
   | Plus _ | Minus _ | Times _ | Div _ ->
   check_local_expr ctx (TInt m) e1
   && check_local_expr ctx (TInt m) e2
   && expected_typ = TInt m
   | Eq _ | Neq _ | Lt _ | Leq _ | Gt _ | Geq _ ->
   check_local_expr ctx (TInt m) e1
   && check_local_expr ctx (TInt m) e2
   && expected_typ = TBool m
   | And _ | Or _ ->
   check_local_expr ctx (TBool m) e1
   && check_local_expr ctx (TBool m) e2
   && expected_typ = TBool m)
   | Let (VarId (var_name, _), local_type, e1, e2, _) ->
   (match check_local_expr ctx local_type e1 with
   | true ->
   let ctx' = add_binding ctx var_name local_type in
   check_local_expr ctx' expected_typ e2
   | _ -> false)
   | Pair (e1, e2, _) ->
   (match expected_typ with
   | TProd (t1, t2, _) -> check_local_expr ctx t1 e1 && check_local_expr ctx t2 e2
   | _ -> false)
   | Fst (e, _) ->
   (match expected_typ with
   | TProd (t1, _, _) -> check_local_expr ctx t1 e
   | _ -> false)
   | Snd (e, _) ->
   (match expected_typ with
   | TProd (_, t2, _) -> check_local_expr ctx t2 e
   | _ -> false)
   | Left (e, _) ->
   (match expected_typ with
   | TSum (t1, _, _) -> check_local_expr ctx t1 e
   | _ -> false)
   | Right (e, _) ->
   (match expected_typ with
   | TSum (_, t2, _) -> check_local_expr ctx t2 e
   | _ -> false)
   | Match (_e, _cases, _) -> true

   and typeof_Val = function
   | Int _ -> TInt m
   | Bool _ -> TBool m
   | String _ -> TString m
   ;; *)

(* ============================== Choreo ============================== *)

(* let rec check_stmts stmts expected_typ =
   let choreo_ctx = [] in
   let global_ctx = [] in
   List.for_all (fun stmt -> check_stmt choreo_ctx global_ctx expected_typ stmt) stmts

   and check_stmt choreo_ctx global_ctx expected_typ = function
   | Ast.Choreo.Decl (_pattn, _choreo_typ, _) -> true
   | Assign (_pattn_ls, expr, _) ->
   check_choreo_expr choreo_ctx global_ctx expected_typ expr
   | TypeDecl (TypId (_id, _), _choreo_typ, _) -> true

   and check_choreo_expr choreo_ctx global_ctx expected_typ = function
   | Unit _ -> expected_typ = Ast.Choreo.TUnit m
   | Var (VarId (var_name, _), _) ->
   (match ctx_lookup choreo_ctx var_name with
   | Ok t -> expected_typ = t
   | _ -> false)
   | LocExpr (LocId (loc_id, _), e, _) ->
   (match expected_typ with
   | TLoc (LocId (loc_id', _), local_typ, _) ->
   loc_id = loc_id'
   && check_local_expr (extract_local_ctx global_ctx loc_id) local_typ e
   | _ -> false)
   | Send (LocId (loc_id1, _), e, LocId (loc_id2, _), _) ->
   (match expected_typ with
   | TLoc (LocId (loc_id2', _), local_typ, _) ->
   check_choreo_expr choreo_ctx global_ctx (TLoc (LocId (loc_id1, m), local_typ, m)) e
   && loc_id2 = loc_id2'
   | _ -> false)
   | Sync (_, _, _, e, _) -> check_choreo_expr choreo_ctx global_ctx expected_typ e
   | If (cond, c1, c2, _) ->
   (match cond with
   | LocExpr (loc_id, _, _) ->
   check_choreo_expr choreo_ctx global_ctx expected_typ c1
   && check_choreo_expr choreo_ctx global_ctx expected_typ c2
   && check_choreo_expr choreo_ctx global_ctx (TLoc (loc_id, TBool m, m)) cond
   | _ -> false)
   | Pair (e1, e2, _) ->
   (match expected_typ with
   | TProd (t1, t2, _) ->
   check_choreo_expr choreo_ctx global_ctx t1 e1
   && check_choreo_expr choreo_ctx global_ctx t2 e2
   | _ -> false)
   | Fst (e, _) ->
   (match expected_typ with
   | TProd (t1, _, _) -> check_choreo_expr choreo_ctx global_ctx t1 e
   | _ -> false)
   | Snd (e, _) ->
   (match expected_typ with
   | TProd (_, t2, _) -> check_choreo_expr choreo_ctx global_ctx t2 e
   | _ -> false)
   | Left (e, _) ->
   (match expected_typ with
   | TSum (t1, _, _) -> check_choreo_expr choreo_ctx global_ctx t1 e
   | _ -> false)
   | Right (e, _) ->
   (match expected_typ with
   | TSum (_, t2, _) -> check_choreo_expr choreo_ctx global_ctx t2 e
   | _ -> false)
   | Match (_e, _ls, _) -> true
   | Let (_stmt_block, _e, _) -> true
   | FunDef (_pattern_ls, _e, _) -> true
   | FunApp (_e1, _e2, _) -> true
   ;; *)
