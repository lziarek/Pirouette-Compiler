open Format

(* ============================== Local ============================== *)

(* ()
   value
   id
   (expr1) bin_op (expr2)
   let var_id := (expr1) in (expr2)
   ((expr1), (expr2))
   fst (expr)
   snd (expr)
   left (expr)
   right (expr)
   match (expr) with
   | case1
   | case2
   | ...
*)
let rec pprint_local_expr ppf (e : Ast.Local.expr) =
  match e with
  | Unit _ -> fprintf ppf "@[<h>()@]"
  | Val (v, _) ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf -> function
        | `Int (i, _) -> fprintf ppf "%d" i
        | `String (s, _) -> fprintf ppf "\"%s\"" s
        | `Bool (b, _) -> fprintf ppf "%b" b)
      v
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | UnOp (op, e, _) ->
    fprintf
      ppf
      "@[<h>%s%a@]"
      (match op with
       | Not _ -> "not "
       | Neg _ -> "-")
      pprint_local_expr
      e
  | BinOp (e1, op, e2, _) ->
    fprintf
      ppf
      "@[<hv2>%a %s@ %a@]"
      pprint_local_expr
      e1
      (match op with
       | Plus _ -> "+"
       | Minus _ -> "-"
       | Times _ -> "*"
       | Div _ -> "/"
       | And _ -> "&&"
       | Or _ -> "||"
       | Eq _ -> "="
       | Neq _ -> "!="
       | Lt _ -> "<"
       | Leq _ -> "<="
       | Gt _ -> ">"
       | Geq _ -> ">=")
      pprint_local_expr
      e2
  | Let (VarId (id, _), e1, e2, _) ->
    fprintf
      ppf
      "@[<hv2>let %s := %a;<1 -2>in@ %a@]"
      id
      pprint_local_expr
      e1
      pprint_local_expr
      e2
  | Pair (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_local_expr e1 pprint_local_expr e2
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst %a@]" pprint_local_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd %a@]" pprint_local_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left %a@]" pprint_local_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right %a@]" pprint_local_expr e
  | Match (e, cases, _) ->
    fprintf
      ppf
      "@[<hv>match %a@ with@[<v2>@ | %a@]@]"
      pprint_local_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_local_case)
      cases

(* pattern -> expr *)
and pprint_local_case ppf (p, e) =
  fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_local_expr e

(* _
   value
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
and pprint_local_pattern ppf = function
  | Default _ -> fprintf ppf "@[<h>_@]"
  | Val (v, _) ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf -> function
        | `Int (i, _) -> fprintf ppf "%d" i
        | `String (s, _) -> fprintf ppf "%s" s
        | `Bool (b, _) -> fprintf ppf "%b" b)
      v
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Pair (p1, p2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_local_pattern p1 pprint_local_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_local_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_local_pattern p

(* unit
   int
   string
   bool
   type * type
   type + type
*)
and pprint_local_type ppf (t : Ast.Local.typ) =
  match t with
  | TUnit _ -> fprintf ppf "@[<h>unit@]"
  | TInt _ -> fprintf ppf "@[<h>int@]"
  | TString _ -> fprintf ppf "@[<h>string@]"
  | TBool _ -> fprintf ppf "@[<h>bool@]"
  | TProd (t1, t2, _) ->
    fprintf ppf "@[<h>%a * %a@]" pprint_local_type t1 pprint_local_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>%a + %a@]" pprint_local_type t1 pprint_local_type t2
;;

(* ============================== Choreo ============================== *)

(* {
      stmt1
      stmt2
      stmt3
      ...
    }
*)
let rec pprint_choreo_stmt_block ppf (stmts : Ast.Choreo.stmt_block) =
  fprintf ppf "@[<v>@[<v>%a@]@]" (pp_print_list pprint_choreo_stmt) stmts

(* (choreo_pattern) : choreo_type
   (choreo_pattern) = (choreo_expr)
   id : choreo_type
*)
and pprint_choreo_stmt ppf = function
  | Decl (p, t, _) ->
    fprintf ppf "@[<h>%a : %a;@]" pprint_choreo_pattern p pprint_choreo_type t
  | Assign (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>%a :=@ %a;@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | TypeDecl (TypId (id, _), t, _) -> fprintf ppf "@[<h>type %s := %a;@]" id pprint_choreo_type t

(* ()
    id
    fst (expr)
    snd (expr)
    left (expr)
    right (expr)
    loc.(local_expr)
    (expr) ~> loc
    loc1[label] ~> loc2; (expr)
    if (condition) then (expr1) else (expr2)
    let {stmt_block} in (expr)
    fun (pattern) -> (expr)
    (fun) (arg)
    (expr1), (expr2)
    match (expr) with
    | case1
    | case2
    | ...
*)
and pprint_choreo_expr ppf = function
  | Unit _ -> fprintf ppf "()"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst@ %a@]" pprint_choreo_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd@ %a@]" pprint_choreo_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_choreo_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_choreo_expr e
  | LocExpr (LocId (loc1, _), e, _) -> fprintf ppf "@[<h>%s.%a@]" loc pprint_local_expr e
  | Send (LocId (loc1, _), e, LocId (loc2, _), _) ->
    fprintf ppf "@[<hv2>[%s] %a@ ~> %s@]" loc1 pprint_choreo_expr e loc2
  | Sync (LocId (loc1, _), LabelId (label, _), LocId (loc2, _), e, _) ->
    fprintf ppf "@[<hv>%s[%s] ~> %s;@ %a@]" loc1 label loc2 pprint_choreo_expr e
  | If (e1, e2, e3, _) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>%a@ then@;<1 2>%a@ else@;<1 2>%a@]"
      pprint_choreo_expr
      e1
      pprint_choreo_expr
      e2
      pprint_choreo_expr
      e3
  | Let (stmt_block, e, _) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ %a@]"
      pprint_choreo_stmt_block
      stmt_block
      pprint_choreo_expr
      e
  | FunDef (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>fun@ %a ->@ %a@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | FunApp (e1, e2, _) ->
    fprintf ppf "@[<hv>%a@ %a@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Pair (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Match (e, cases, _) ->
    fprintf
      ppf
      "@[<v>match %a @ with@ | %a@]"
      pprint_choreo_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_choreo_case)
      cases

(* pattern -> expr *)
and pprint_choreo_case ppf (p, e) =
  fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_choreo_pattern p pprint_choreo_expr e

(* _
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
and pprint_choreo_pattern ppf = function
  | Default _ -> fprintf ppf "@[<h>_@]"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | LocPatt (LocId (loc, _), p, _) -> fprintf ppf "@[<h>%s.%a@]" loc pprint_local_pattern p
  | Pair (p1, p2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_choreo_pattern p1 pprint_choreo_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_choreo_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_choreo_pattern p

(*
   unit
   loc.(local_type)
   type -> type
   type * type
   type + type
*)
and pprint_choreo_type ppf = function
  | TUnit _ -> fprintf ppf "@[<h>()@]"
  | TLoc (LocId (loc, _), t, _) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_type t
  | TMap (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) -> (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TProd (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TAlias (TypId (id, _), t, _)-> fprintf ppf "@[<h>type %s := %a;@]" id pprint_choreo_type t
;;

(* ============================== Net ============================== *)

let rec pprint_net_stmt_block ppf (stmts : Ast.Net.stmt_block) =
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]" (pp_print_list pprint_net_stmt) stmts

and pprint_net_stmt ppf = function
  | Decl (p, t) -> fprintf ppf "@[<h>(%a) : %a@]" pprint_local_pattern p pprint_net_type t
  | Assign (ps, e) ->
    fprintf
      ppf
      "@[<hv2>(%a) :=@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | TypeDecl (TypId (id, _), t) -> fprintf ppf "@[<h>%s : %a@]" id pprint_net_type t

and pprint_net_expr ppf = function
  | Unit -> fprintf ppf "@[<h>()@]"
  | Var (VarId (id, _)) -> fprintf ppf "@[<h>%s@]" id
  | Ret e -> fprintf ppf "@[<hv2>ret@ (%a)@]" pprint_local_expr e
  | Fst e -> fprintf ppf "@[<hv2>fst@ (%a)@]" pprint_net_expr e
  | Snd e -> fprintf ppf "@[<hv2>snd@ (%a)@]" pprint_net_expr e
  | Left e -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_net_expr e
  | Right e -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_net_expr e
  | Send (e, LocId (loc, _)) -> fprintf ppf "@[<hv2>(%a) ~> %s@]" pprint_net_expr e loc
  | Recv (LocId (loc, _)) -> fprintf ppf "@[<~ %s@]" loc
  | If (e1, e2, e3) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
      pprint_net_expr
      e1
      pprint_net_expr
      e2
      pprint_net_expr
      e3
  | Let (stmt_block, e) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ (%a)@]"
      pprint_net_stmt_block
      stmt_block
      pprint_net_expr
      e
  | FunDef (ps, e) ->
    fprintf
      ppf
      "@[<hv2>fun@ (%a) ->@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | FunApp (e1, e2) ->
    fprintf ppf "@[<hv>(%a)@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Pair (e1, e2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Match (e, cases) ->
    fprintf
      ppf
      "@[<hv>match (%a)@ with@[<v2>@ | %a@]@]"
      pprint_net_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_case)
      cases
  | ChooseFor (LabelId (id, _), LocId (locid, _), e) ->
    fprintf ppf "@[<hv>choose %s for %s;@ %a@]" id locid pprint_net_expr e
  | AllowChoice (LocId (loc, _), choices) ->
    fprintf
      ppf
      "@[<v>allow %s choice@ | %a@]"
      loc
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_choice)
      choices

and pprint_net_case ppf (p, e) =
  fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_net_expr e

and pprint_net_choice ppf (LabelId (id, _), e) =
  fprintf ppf "@[<hv2>%s ->@ %a@]" id pprint_net_expr e

and pprint_net_type ppf = function
  | TUnit -> fprintf ppf "@[<h>unit@]"
  | TLoc t -> fprintf ppf "@[<h>%a@]" pprint_local_type t
  | TMap (t1, t2) ->
    fprintf ppf "@[<h>(%a) -> (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TProd (t1, t2) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TSum (t1, t2) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_net_type t1 pprint_net_type t2
;;
