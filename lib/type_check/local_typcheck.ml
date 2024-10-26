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

(*dummy metainfo to let compiler happy*)
let _m = Ok "dummy"

(*generate unique free type variables*)
let fresh_ftv =
  let counter = ref 0 in
  fun () ->
    let v = !counter in
    counter := !counter + 1;
    "T" ^ string_of_int v
;;

let rec unify (t1 : ftv Local.typ) (t2 : ftv Local.typ) : substitution =
  (*How to unify a variable with concrete type in this unify function?*)
  (*Local.typ doesn't have Var.*)
  match t1, t2 with
  | Local.TInt _, Local.TInt _
  | Local.TBool _, Local.TBool _
  | Local.TString _, Local.TString _
  | Local.TUnit _, Local.TUnit _ -> []
  | Local.TProd (t1, t2, _), Local.TProd (t1', t2', _) ->
    let s1 = unify t1 t1' in
    let s2 = unify t2 t2' in
    s1 @ s2
  | Local.TSum (t1, t2, _), Local.TSum (t1', t2', _) ->
    let s1 = unify t1 t1' in
    let s2 = unify t2 t2' in
    s1 @ s2
  | _ -> failwith "Unification failed"
;;

let rec apply_subst = ()

(*traverse the substitution list `s`, apply sth to `t` ?*)
let apply_sub_typ _s _t = ()
(*traverse the substitution list `s`, apply sth to `t` ?*)

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

let rec infer_local_expr local_ctx typ : substitution * ftv Local.typ =
  match typ with
  | Local.Unit _ ->
    let ftv_name = fresh_ftv () in
    [ ftv_name, Local.TUnit (Ok ftv_name) ], Local.TUnit (Ok ftv_name)
  | Local.Val (v, _) ->
    let ftv_name = fresh_ftv () in
    [ ftv_name, typeof_Val v ], typeof_Val v
  | Local.Var (Local.VarId (var_name, _), _) ->
    (match ctx_lookup local_ctx var_name with
     | Ok t -> [], t
     | _ -> failwith "Variable not found")
  | Local.UnOp (op, e, _) ->
    (match op with
     | Local.Neg _ ->
       let s, t = infer_local_expr local_ctx e in
       (*1. should we apply s to t to get a t'?*)
       (*unify the `t` with int becasue Neg is int*)
       let unify_subst = unify t (Local.TInt _m) in
       s, Local.TInt _m
     | Local.Not _ -> infer_local_expr local_ctx e)
  | _ -> [], Local.TUnit _m

and typeof_Val = function
  | Int _ -> TInt (Ok "TInt")
  | Bool _ -> TBool (Ok "TBool")
  | String _ -> TString (Ok "TString")
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
