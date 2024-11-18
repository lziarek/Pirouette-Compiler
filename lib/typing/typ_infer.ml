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
type global_ctx = (string * string * ftv Local.typ) list

(*free type variables*)
let (m : ftv) = Ok "dummy info"

let rec unify t1 t2 : local_subst =
  match t1, t2 with
  | Local.TInt _, Local.TInt _
  | Local.TBool _, Local.TBool _
  | Local.TString _, Local.TString _
  | Local.TUnit _, Local.TUnit _ -> []
  | Local.TVar (Local.TypId (var_name, _), _), t
  | t, Local.TVar (Local.TypId (var_name, _), _) ->
    if t = Local.TVar (Local.TypId (var_name, m), m)
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

and unify_choreo (t1 : ftv Choreo.typ) (t2 : ftv Choreo.typ) : choreo_subst =
  match t1, t2 with
  | Choreo.TProd (t1a, t1b, _), Choreo.TProd (t2a, t2b, _)
  | Choreo.TSum (t1a, t1b, _), Choreo.TSum (t2a, t2b, _) ->
    let s1 = unify_choreo t1a t2a in
    let s2 =
      unify_choreo (apply_subst_typ_choreo s1 t1b) (apply_subst_typ_choreo s1 t2b)
    in
    s1 @ s2
  | _ -> failwith "Unification failed"

(* Apply substitution to a Choreo.typ *)
and apply_subst_typ_choreo (s : choreo_subst) (t : ftv Choreo.typ) : ftv Choreo.typ =
  match t with
  | Choreo.TProd (t1, t2, _) ->
    Choreo.TProd (apply_subst_typ_choreo s t1, apply_subst_typ_choreo s t2, m)
  | Choreo.TSum (t1, t2, _) ->
    Choreo.TSum (apply_subst_typ_choreo s t1, apply_subst_typ_choreo s t2, m)
  | _ -> failwith "Type not found when applying substitution"

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
     | Not_found -> t)
  | Local.TProd (t1, t2, _) -> Local.TProd (apply_subst_typ s t1, apply_subst_typ s t2, m)
  | Local.TSum (t1, t2, _) -> Local.TSum (apply_subst_typ s t1, apply_subst_typ s t2, m)

(*apply substitution to context*)
and apply_subst_ctx subst ctx =
  List.map (fun (var_name, var_type) -> var_name, apply_subst_typ subst var_type) ctx

(*apply substitution to choreo context *)
and apply_subst_ctx_choreo (subst : choreo_subst) (ctx : choreo_ctx) : choreo_ctx =
  List.map
    (fun (var_name, var_type) -> var_name, apply_subst_typ_choreo subst var_type)
    ctx

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
and extract_local_ctx (global_ctx : global_ctx) loc_id =
  List.fold_right
    (fun (loc, var_name, typ) acc -> if loc = loc_id then (var_name, typ) :: acc else acc)
    global_ctx
    []

(*return a choreo sub given local substitution and loc_id*)
and get_choreo_sub local_subst loc_id =
  List.fold_right
    (fun (var_name, typ) acc -> (var_name, Choreo.TLoc (loc_id, typ, m)) :: acc)
    local_subst
    []

(*return a choreo context given local context and loc_id*)
and get_choreo_ctx local_ctx loc_id =
  List.fold_right
    (fun (var_name, typ) acc -> (var_name, Choreo.TLoc (loc_id, typ, m)) :: acc)
    local_ctx
    []
;;

(* ============================== Local ============================== *)

let rec infer_local_expr local_ctx = function
  | Local.Unit _ -> [], Local.TUnit m
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
       let s2 = unify t' (Local.TInt m) in
       s1 @ s2, Local.TInt m
     | Local.Not _ ->
       let s1, t = infer_local_expr local_ctx e in
       let t' = apply_subst_typ s1 t in
       let s2 = unify t' (Local.TBool m) in
       s1 @ s2, Local.TBool m)
  | Local.BinOp (e1, op, e2, _) ->
    (match op with
     | Local.Plus _ | Local.Minus _ | Local.Times _ | Local.Div _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt m) in
       let s4 = unify t2' (Local.TInt m) in
       s1 @ s2 @ s3 @ s4, Local.TInt m
     | Local.Eq _ | Local.Neq _ | Local.Lt _ | Local.Leq _ | Local.Gt _ | Local.Geq _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt m) in
       let s4 = unify t2' (Local.TInt m) in
       s1 @ s2 @ s3 @ s4, Local.TBool m
     | Local.And _ | Local.Or _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TBool m) in
       let s4 = unify t2' (Local.TBool m) in
       s1 @ s2 @ s3 @ s4, Local.TBool m)
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
    let comb_subst = s1 @ s2 in
    ( comb_subst
    , Local.TProd (apply_subst_typ comb_subst t1, apply_subst_typ comb_subst t2, m) )
  | Local.Fst (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    let t' = apply_subst_typ s1 t in
    (match t' with
     | Local.TProd (t1, _, _) -> s1, t1
     | _ -> failwith "Fst Type error")
  | Local.Snd (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    let t' = apply_subst_typ s1 t in
    (match t' with
     | Local.TProd (_, t2, _) -> s1, t2
     | _ -> failwith "Snd Type error")
  | Local.Left (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    let t' = apply_subst_typ s1 t in
    s1, Local.TSum (t', Local.TVar (Local.TypId (gen_ftv (), m), m), m)
  | Local.Right (e, _) ->
    let s1, t = infer_local_expr local_ctx e in
    let t' = apply_subst_typ s1 t in
    s1, Local.TSum (Local.TVar (Local.TypId (gen_ftv (), m), m), t', m)
  | Local.Match (e, cases, _) ->
    (*infer cases*)
    let patt_ls, expr_ls = List.split cases in
    (*infer patterns to get list of each subst, each types, each ctx*)
    let ls = List.map (fun patt -> infer_local_pattern local_ctx patt) patt_ls in
    let s1, t_ls, ctx_ls =
      List.fold_right
        (fun (s, t, ctx) (s_acc, t_acc, ctx_acc) -> s @ s_acc, t :: t_acc, ctx :: ctx_acc)
        ls
        ([], [], [])
    in
    (* for each type, apply subst to rewrite the types*)
    let t_ls' = List.map (fun t -> apply_subst_typ s1 t) t_ls in
    (match t_ls', e with
     (*fuse types of patterns to get type of e*)
     | [ TSum (t1, TVar _, _); TSum (TVar _, t2, _) ], Var (Local.VarId (e_name, _), _)
     | [ TSum (TVar _, t2, _); TSum (t1, TVar _, _) ], Var (Local.VarId (e_name, _), _) ->
       let t_match = Local.TSum (t1, t2, m) in
       (*add the type binding into ctx so sub expr know the type of e*)
       let local_ctx' = (e_name, t_match) :: local_ctx in
       (*for each sub exprs, infer it with contexts from each patterns*)
       (*apply, unify, return typ of expr*)
       (*also check if type mismatch during list of patterns and exprs*)
       (*infer each sub expr by each ctx@local_ctx, get a list of substs with list of types*)
       let s2, typ_ls =
         List.fold_right
           (fun (expr, ctx) (s_acc, typ_acc) ->
             let s, typ = infer_local_expr (apply_subst_ctx s1 ctx @ local_ctx') expr in
             s @ s_acc, typ :: typ_acc)
           (List.combine expr_ls ctx_ls)
           ([], [])
       in
       let typ_ls' = List.map (fun t -> apply_subst_typ s2 t) typ_ls in
       (*if all the sub expression return the same type, unify them and return*)
       if List.for_all (fun t -> t = List.hd typ_ls') typ_ls'
       then (
         let s3 =
           List.fold_left (fun acc t -> unify t (List.hd typ_ls') @ acc) [] typ_ls'
         in
         s1 @ s2 @ s3, List.hd typ_ls')
       else failwith "Type of sub exprs mismatch"
     | _ -> failwith "Type of patterns are not sum types")

and typeof_Val = function
  | Int _ -> TInt m
  | Bool _ -> TBool m
  | String _ -> TString m

and infer_local_pattern local_ctx = function
  | Local.Default _ -> [], Local.TUnit m, []
  | Local.Val (v, _) -> [], typeof_Val v, []
  | Local.Var (Local.VarId (var_name, _), _) ->
    let typ_v = Local.TVar (Local.TypId (gen_ftv (), m), m) in
    [], typ_v, [ var_name, typ_v ]
  | Local.Pair (p1, p2, _) ->
    let s1, t1, ctx1 = infer_local_pattern local_ctx p1 in
    let s2, t2, ctx2 = infer_local_pattern (apply_subst_ctx s1 local_ctx) p2 in
    let comb_subst = s1 @ s2 in
    ( comb_subst
    , Local.TProd (apply_subst_typ comb_subst t1, apply_subst_typ comb_subst t2, m)
    , ctx1 @ ctx2 )
  | Local.Left (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (t, Local.TVar (Local.TypId (gen_ftv (), m), m), m), ctx
  | Local.Right (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (Local.TVar (Local.TypId (gen_ftv (), m), m), t, m), ctx
;;

(* ============================== Choreo ============================== *)

let rec infer_choreo_stmt choreo_ctx global_ctx stmt : choreo_subst * ftv Choreo.typ * choreo_ctx =
  match stmt with
  | Choreo.Decl (pattn, choreo_typ, _) -> 
    (*let x : Alice.int*) (*infer pattern type of x, then bind it to Alice, add it to choreo ctx, and return it
    should be Var *)
    let s,t,ctx = infer_choreo_pattern choreo_ctx global_ctx pattn in 
    let t' = apply_subst_typ_choreo s t in
    (match t' with 
    )
    s, t', ctx
  | Choreo.Assign (pattn_ls, expr, _) -> infer_choreo_expr choreo_ctx global_ctx expr
  | Choreo.TypeDecl (TypId (id, _), choreo_typ, _) -> failwith "Not implemented"

and infer_choreo_stmt_block _stmts = failwith "Not implemented"

and infer_choreo_expr choreo_ctx (global_ctx : global_ctx) = function
  | Choreo.Unit _ -> [], Choreo.TUnit m
  | Choreo.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup choreo_ctx var_name with
     | Ok t -> [], t
     | _ -> failwith "Variable not found when inferring expression")
  | Choreo.LocExpr (Local.LocId (loc_id, _), local_expr, _) ->
    let local_ctx = extract_local_ctx global_ctx loc_id in
    let local_s, t = infer_local_expr local_ctx local_expr in
    let t' = apply_subst_typ local_s t in
    let choreo_sub = get_choreo_sub local_s (Local.LocId (loc_id, m)) in
    choreo_sub, Choreo.TLoc (Local.LocId (loc_id, m), t', m)
  | Choreo.Send (Local.LocId (src, _), e, Local.LocId (dst, _), _) ->
    let s1, t = infer_choreo_expr choreo_ctx global_ctx e in
    let t' = apply_subst_typ_choreo s1 t in
    (match t' with
     (*check if the loc_id matches with the src location id*)
     | Choreo.TLoc (Local.LocId (loc_id, _), local_t, _) ->
       if loc_id = src
       then (
         let s2 = unify_choreo t' (Choreo.TLoc (Local.LocId (src, m), local_t, m)) in
         s1 @ s2, Choreo.TLoc (Local.LocId (dst, m), local_t, m))
       else failwith "Source location mismatch"
     | _ -> failwith "Type mismatch")
  | Choreo.Sync (_, _, _, e, _) -> infer_choreo_expr choreo_ctx global_ctx e
  | Choreo.If (cond, e1, e2, _) ->
    let s_cond, t_cond = infer_choreo_expr choreo_ctx global_ctx cond in
    (match t_cond with
     | Choreo.TLoc (_, Local.TBool _, _) ->
       let s1, t1 =
         infer_choreo_expr (apply_subst_ctx_choreo s_cond choreo_ctx) global_ctx e1
       in
       let s2, t2 =
         infer_choreo_expr (apply_subst_ctx_choreo s1 choreo_ctx) global_ctx e2
       in
       let s3 = unify_choreo t1 t2 in
       s_cond @ s1 @ s2 @ s3, apply_subst_typ_choreo s3 t2
     | _ -> failwith "Expected boolean type")
  | Choreo.Let (stmts, e, _) ->
    let s1, t1 = infer_choreo_stmt_block stmts in
    let s2, t2 = infer_choreo_expr (apply_subst_ctx_choreo s1 choreo_ctx) global_ctx e in
    s1 @ s2, apply_subst_typ_choreo s2 t1
  | Choreo.FunDef (patterns, e, _) ->
    let s1, t1s, ctx =
      List.fold_right
        (fun pat (s_acc, t_acc, ctx_acc) ->
          let s, t, c = infer_choreo_pattern choreo_ctx global_ctx pat in
          s @ s_acc, t :: t_acc, c @ ctx_acc)
        patterns
        ([], [], [])
    in
    let s2, t2 = infer_choreo_expr (ctx @ choreo_ctx) global_ctx e in
    s1 @ s2, List.fold_right (fun t1 t2 -> Choreo.TMap (t1, t2, m)) t1s t2
  | Choreo.FunApp (func, param, _) ->
    let s1, func_typ = infer_choreo_expr choreo_ctx global_ctx func in
    let s2, param_typ =
      infer_choreo_expr (apply_subst_ctx_choreo s1 choreo_ctx) global_ctx param
    in
    let func_typ' = apply_subst_typ_choreo s2 func_typ in
    (match func_typ' with
     | Choreo.TMap (function_input_typ, return_typ, _) ->
       (*if function type is correct and param type is correct, then everything is correct
         also check for *)
       let s3 = unify_choreo function_input_typ param_typ in
       s1 @ s2 @ s3, return_typ
     | _ -> failwith "Expected function type")
  | Choreo.Pair (e1, e2, _) ->
    let s1, t1 = infer_choreo_expr choreo_ctx global_ctx e1 in
    let s2, t2 = infer_choreo_expr (apply_subst_ctx_choreo s1 choreo_ctx) global_ctx e2 in
    let comb_subst = s1 @ s2 in
    ( comb_subst
    , Choreo.TProd
        (apply_subst_typ_choreo comb_subst t1, apply_subst_typ_choreo comb_subst t2, m) )
  | Choreo.Fst (e, _) ->
    let s, t = infer_choreo_expr choreo_ctx global_ctx e in
    let t' = apply_subst_typ_choreo s t in
    (match t' with
     | Choreo.TProd (t1, _, _) -> s, t1
     | _ -> failwith "Expected product type")
  | Choreo.Snd (e, _) ->
    let s, t = infer_choreo_expr choreo_ctx global_ctx e in
    let t' = apply_subst_typ_choreo s t in
    (match t' with
     | Choreo.TProd (_, t2, _) -> s, t2
     | _ -> failwith "Expected product type")
  | Choreo.Left (e, _) ->
    let s, t = infer_choreo_expr choreo_ctx global_ctx e in
    let t' = apply_subst_typ_choreo s t in
    ( s
    , Choreo.TSum
        ( t'
        , Choreo.TLoc
            (Local.LocId ("dummy", m), Local.TVar (Local.TypId (gen_ftv (), m), m), m)
        , m ) )
  | Choreo.Right (e, _) ->
    let s, t = infer_choreo_expr choreo_ctx global_ctx e in
    let t' = apply_subst_typ_choreo s t in
    ( s
    , Choreo.TSum
        ( Choreo.TLoc
            (Local.LocId ("dummy", m), Local.TVar (Local.TypId (gen_ftv (), m), m), m)
        , t'
        , m ) )
  | Choreo.Match (e, cases, _) ->
    let s1, t_match = infer_choreo_expr choreo_ctx global_ctx e in
    let pats, exprs = List.split cases in
    let s2, ts, ctxs =
      List.fold_right
        (fun pat (s_acc, t_acc, ctx_acc) ->
          let s, t, c = infer_choreo_pattern choreo_ctx global_ctx pat in
          s @ s_acc, t :: t_acc, c :: ctx_acc)
        pats
        ([], [], [])
    in
    let s3 =
      List.fold_right
        (fun t acc -> unify_choreo t t_match @ acc)
        (List.map (apply_subst_typ_choreo s2) ts)
        []
    in
    let s4, types =
      List.fold_right2
        (fun expr ctx (s_acc, t_acc) ->
          let s, t = infer_choreo_expr (ctx @ choreo_ctx) global_ctx expr in
          s @ s_acc, t :: t_acc)
        exprs
        ctxs
        ([], [])
    in
    let s5 =
      List.fold_left (fun acc t -> unify_choreo t (List.hd types) @ acc) [] types
    in
    s1 @ s2 @ s3 @ s4 @ s5, apply_subst_typ_choreo s5 (List.hd types)

and infer_choreo_pattern choreo_ctx global_ctx = function
  | Choreo.Default _ -> [], Choreo.TUnit m, []
  | Choreo.Var (Local.VarId (var_name, _), _) ->
    let typ_v = Local.TVar (Local.TypId (gen_ftv (), m), m) in
    [], typ_v, [ var_name, typ_v ]
  | Choreo.Pair (p1, p2, _) ->
    let s1, t1, ctx1 = infer_choreo_pattern choreo_ctx global_ctx p1 in
    let s2, t2, ctx2 =
      infer_choreo_pattern (apply_subst_ctx_choreo s1 choreo_ctx) global_ctx p2
    in
    let comb_subst = s1 @ s2 in
    ( comb_subst
    , Choreo.TProd
        (apply_subst_typ_choreo comb_subst t1, apply_subst_typ_choreo comb_subst t2, m)
    , ctx1 @ ctx2 )
  | Choreo.LocPat (Local.LocId (loc_id, _), local_pat, _) ->
    let local_ctx = extract_local_ctx global_ctx loc_id in
    let local_s, t, ctx = infer_local_pattern local_ctx local_pat in
    let t' = apply_subst_typ local_s t in
    let choreo_sub = get_choreo_sub local_s (Local.LocId (loc_id, m)) in
    let choreo_ctx = get_choreo_ctx ctx (Local.LocId (loc_id, m)) in
    choreo_sub, Choreo.TLoc (Local.LocId (loc_id, m), t', m), choreo_ctx
  | Choreo.Left (p, _) ->
    let s, t, ctx = infer_choreo_pattern choreo_ctx global_ctx p in
    ( s
    , Choreo.TSum
        ( t
        , Choreo.TLoc
            (Local.LocId ("dummy_loc", m), Local.TVar (Local.TypId (gen_ftv (), m), m), m)
        , m )
    , ctx )
  | Choreo.Right (p, _) ->
    let s, t, ctx = infer_choreo_pattern choreo_ctx global_ctx p in
    ( s
    , Choreo.TSum
        ( Choreo.TLoc
            (Local.LocId ("dummy_loc", m), Local.TVar (Local.TypId (gen_ftv (), m), m), m)
        , t
        , m )
    , ctx )
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
