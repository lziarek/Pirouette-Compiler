(* ============================== Types ============================== *)
module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M

(*type inference*)
type errmsg = string
type typvar = string
type ftv = (typvar, errmsg) result
type substitution = (typvar * ftv Local.typ) list

(*context: list of pair of variable name and its binding*)
type local_context = (string * ftv Local.typ) list
type choreo_context = (string * ftv Choreo.typ) list
type global_context = (string * ftv Local.typ) list

(*free type variables*)
let (_m : ftv) = Ok "dummy"
let (tInt : ftv) = Ok "int"
let (tBool : ftv) = Ok "bool"
let (tString : ftv) = Ok "string"
let (tUnit : ftv) = Ok "unit"

(* ============================== Global ============================== *)

(*generate unique free type variables*)
let gen_ftv =
  let counter = ref 0 in
  fun () ->
    let v = !counter in
    counter := !counter + 1;
    "T" ^ string_of_int v
;;

let rec unify t1 t2 =
  (*How to unify a variable with concrete type in this unify function?*)
  (*Local.typ doesn't have Var.*)
  match t1, t2 with
  | Local.TInt _, Local.TInt _
  | Local.TBool _, Local.TBool _
  | Local.TString _, Local.TString _
  | Local.TUnit _, Local.TUnit _ -> []
  | Local.TProd (t1a, t1b, _), Local.TProd (t2a, t2b, _)
  | Local.TSum (t1a, t1b, _), Local.TSum (t2a, t2b, _) ->
    let s1 = unify t1a t2a in
    let s2 = unify (apply_subst_typ s1 t1b) (apply_subst_typ s1 t2b) in
    s1 @ s2
  | _ -> failwith "Unification failed"

(*traverse the substitution list `s`, apply all occurences of subst to `t`*)
(*but we don't have var in Local.typ*)
and apply_subst_typ s t =
  match t with
  | (Local.TInt _ | Local.TBool _ | Local.TString _ | Local.TUnit _) as t' -> t'
  | Local.TProd (t1, t2, _) -> Local.TProd (apply_subst_typ s t1, apply_subst_typ s t2, _m)
  | Local.TSum (t1, t2, _) -> Local.TSum (apply_subst_typ s t1, apply_subst_typ s t2, _m)

(*apply substitution to context*)
and apply_subst_ctx subst ctx =
  List.map (fun (var_name, var_type) -> var_name, apply_subst_typ subst var_type) ctx
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
  | Local.Unit _ -> [], Local.TUnit tUnit
  | Local.Val (v, _) -> [], typeof_Val v
  | Local.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup local_ctx var_name with
     | Ok t -> [], t
     | _ -> failwith "Variable not found")
  | Local.UnOp (op, e, _) ->
    (match op with
     | Local.Neg _ ->
       let s1, t = infer_local_expr local_ctx e in
       let t' = apply_subst_typ s1 t in
       let s2 = unify t' (Local.TInt tInt) in
       s1 @ s2, Local.TInt tInt
     | Local.Not _ ->
       let s1, t = infer_local_expr local_ctx e in
       let t' = apply_subst_typ s1 t in
       let s2 = unify t' (Local.TBool tBool) in
       s1 @ s2, Local.TBool tBool)
  | Local.BinOp (e1, op, e2, _) ->
    (match op with
     | Local.Plus _ | Local.Minus _ | Local.Times _ | Local.Div _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt tInt) in
       let s4 = unify t2' (Local.TInt tInt) in
       s1 @ s2 @ s3 @ s4, Local.TInt tInt
     | Local.Eq _ | Local.Neq _ | Local.Lt _ | Local.Leq _ | Local.Gt _ | Local.Geq _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TInt tInt) in
       let s4 = unify t2' (Local.TInt tInt) in
       s1 @ s2 @ s3 @ s4, Local.TBool tBool
     | Local.And _ | Local.Or _ ->
       let s1, t1 = infer_local_expr local_ctx e1 in
       let s2, t2 = infer_local_expr (apply_subst_ctx s1 local_ctx) e2 in
       let t1' = apply_subst_typ s1 t1 in
       let t2' = apply_subst_typ s2 t2 in
       let s3 = unify t1' (Local.TBool tBool) in
       let s4 = unify t2' (Local.TBool tBool) in
       s1 @ s2 @ s3 @ s4, Local.TBool tBool)
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
  | Local.Match (_e, _cases, _) -> failwith "Match not implemented"

and typeof_Val = function
  | Int _ -> TInt tInt
  | Bool _ -> TBool tBool
  | String _ -> TString tString
;;

let rec infer_local_pattern local_ctx = function
  | Local.Default _ -> [], Local.TUnit tUnit, []
  | Local.Val (v, _) -> [], typeof_Val v, []
  | Local.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup local_ctx var_name with
     | Ok t -> [], t, [ var_name, t ]
     | _ -> failwith "Variable not found")
  | Local.Pair (p1, p2, _) ->
    let s1, t1, ctx1 = infer_local_pattern local_ctx p1 in
    let s2, t2, ctx2 = infer_local_pattern (apply_subst_ctx s1 local_ctx) p2 in
    s1 @ s2, Local.TProd (apply_subst_typ s2 t1, apply_subst_typ s2 t2, _m), ctx1 @ ctx2
  | Local.Left (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (t, Local.TVar (gen_ftv ()) , _m), ctx
  | Local.Right (p, _) ->
    let s, t, ctx = infer_local_pattern local_ctx p in
    s, Local.TSum (Local.TVar (gen_ftv ()), t, _m), ctx
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
