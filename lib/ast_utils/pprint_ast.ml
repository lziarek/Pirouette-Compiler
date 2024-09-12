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
      "@[<h>%s(%a)@]"
      (match op with
       | Not _ -> "not "
       | Neg _ -> "-")
      pprint_local_expr
      e
  | BinOp (e1, op, e2, _) ->
    fprintf
      ppf
      "@[<hv2>(%a) %s@ (%a)@]"
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
      "@[<hv2>let %s :=@ (%a)@;<1 -2>in@ (%a)@]"
      id
      pprint_local_expr
      e1
      pprint_local_expr
      e2
  | Pair (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_local_expr e1 pprint_local_expr e2
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst (%a)@]" pprint_local_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd (%a)@]" pprint_local_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left (%a)@]" pprint_local_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right (%a)@]" pprint_local_expr e
  | Match (e, cases, _) ->
    fprintf
      ppf
      "@[<hv>match (%a)@ with@[<v2>@ | %a@]@]"
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
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_local_pattern p1 pprint_local_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_local_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_local_pattern p

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
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_local_type t1 pprint_local_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_local_type t1 pprint_local_type t2
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
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]" (pp_print_list pprint_choreo_stmt) stmts

(* (choreo_pattern) : choreo_type
   (choreo_pattern) = (choreo_expr)
   id : choreo_type
*)
and pprint_choreo_stmt ppf = function
  | Decl (p, t, _) ->
    fprintf ppf "@[<h>(%a) : %a;@]" pprint_choreo_pattern p pprint_choreo_type t
  | Assign (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>(%a) :=@ (%a);@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | TypeDecl (TypId (id, _), t, _) -> fprintf ppf "@[<h>%s : %a@]" id pprint_choreo_type t

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
  | Unit _ -> fprintf ppf "Unit"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst@ (%a)@]" pprint_choreo_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd@ (%a)@]" pprint_choreo_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_choreo_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_choreo_expr e
  | LocExpr (LocId (loc, _), e, _) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_expr e
  | Send (LocId (loc1, _), e, LocId (loc2, _), _) ->
    fprintf ppf "@[<hv2>[%s] (%a)@ ~> %s@]" loc1 pprint_choreo_expr e loc2
  | Sync (LocId (loc1, _), LabelId (label, _), LocId (loc2, _), e, _) ->
    fprintf ppf "@[<hv>%s[%s] ~> %s;@ (%a)@]" loc1 label loc2 pprint_choreo_expr e
  | If (e1, e2, e3, _) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
      pprint_choreo_expr
      e1
      pprint_choreo_expr
      e2
      pprint_choreo_expr
      e3
  | Let (stmt_block, e, _) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ (%a)@]"
      pprint_choreo_stmt_block
      stmt_block
      pprint_choreo_expr
      e
  | FunDef (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>fun@ (%a) ->@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | FunApp (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a)@ (%a)@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Pair (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Match (e, cases, _) ->
    fprintf
      ppf
      "@[<v>match (%a)@ with@ | %a@]"
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
  | LocPatt (LocId (loc, _), p, _) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_pattern p
  | Pair (p1, p2, _) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_choreo_pattern p1 pprint_choreo_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_choreo_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_choreo_pattern p

(*
   unit
   loc.(local_type)
   type -> type
   type * type
   type + type
*)
and pprint_choreo_type ppf = function
  | TUnit _ -> fprintf ppf "@[<h>unit@]"
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

(* ============================== ethan-vincent/pretty-printer ============================== *)
(* Choreo pp*)

(** [pprint_choreo_stmt] takes a formatter [fmt] and a statement [statement] and prints the formatted code of the statement

    - Variants of statements include Decl, Assign, TypeDecl
    - Calls helper functions [pp_choreo_pattern], [pp_choreo_typ], [pp_choreo_expr] to pretty print the statement *)

let rec pprint_choreo_stmt fmt (statement : Ast.Choreo.stmt) =
  match statement with
  | Decl (patn, typ) ->
    fprintf fmt "@[<h>%a : %a;@]" pp_choreo_pattern patn pp_choreo_typ typ
  | Assign (patn_list, expr) ->
    fprintf fmt "@[<h>%a :=@ %a;@]" pp_choreo_pattns patn_list pp_choreo_expr expr
  | TypeDecl (TypId id, typ) -> fprintf fmt "type %s := %a;\n" id pp_choreo_typ typ

and pp_choreo_pattns fmt patn_list =
  pp_print_list ~pp_sep:pp_print_space pp_choreo_pattern fmt patn_list

(** [pp_choreo_pattern] takes a formatter [fmt] and a choreo pattern [patn] and prints the formatted code of the choreo pattern

    - Variants of patterns include Default, LocPatt, Var, Left, Right, and Pair
    - For variant [LocPatt], it calls helper function [pp_local_pattern] to pretty print the local pattern
    - For variants [Pair], [Left], and [Right], it calls helper function [pp_choreo_pattern] to pretty print the choreo pattern *)
and pp_choreo_pattern fmt patn =
  match patn with
  | Default -> fprintf fmt "_"
  | LocPatt (LocId loc, lp) -> fprintf fmt "%s.%a" loc pp_local_pattern lp
  | Var (VarId id) -> fprintf fmt "%s" id
  | Left patn -> fprintf fmt "left %a" pp_choreo_pattern patn
  | Right patn -> fprintf fmt "right %a" pp_choreo_pattern patn
  | Pair (patn1, patn2) ->
    fprintf fmt "(%a, %a)" pp_choreo_pattern patn1 pp_choreo_pattern patn2

(** [pp_choreo_typ] takes a formatter [fmt] and a choreo type [choreo_typ] and prints the formatted code of the choreo type

    - Variants of choreo types include TUnit, TLoc, TSend, TProd, and TSum
    - For variant [TLoc], it calls helper function [pp_local_typ] to pretty print the local type
    - For variants [TSend], [TProd], and [TSum], it calls helper function [pp_choreo_typ] to pretty print the choreo type *)
and pp_choreo_typ fmt choreo_typ =
  match choreo_typ with
  | TUnit -> fprintf fmt "unit"
  | TLoc (LocId id, typ) -> fprintf fmt "%s.(%a)" id pp_local_typ typ
  | TSend (typ1, typ2) -> fprintf fmt "%a -> %a" pp_choreo_typ typ1 pp_choreo_typ typ2
  | TProd (typ1, typ2) -> fprintf fmt "%a * %a" pp_choreo_typ typ1 pp_choreo_typ typ2
  | TSum (typ1, typ2) -> fprintf fmt "%a + %a" pp_choreo_typ typ1 pp_choreo_typ typ2

(** [pp_choreo_expr] takes a formatter [fmt] and a choreo expression [expr] and prints the formatted code of the choreo expression

    - Variants of choreo expressions include Unit, Var, LocExpr, Send, Sync, If, Let, FunDef, FunApp,
      Pair, Fst, Snd, Left, Right, and Match
    - For variant [LocExpr], it calls helper function [pp_local_expr] to pretty print the local expression
    - For variant [Let], it calls helper function [pp_stmts] to pretty print the declaration block
    - For variant [Match], it calls helper function [pp_choreo_cases] to pretty print the choreo cases *)
and pp_choreo_expr fmt expr =
  match expr with
  | Unit -> fprintf fmt "()"
  | Var (VarId id) -> fprintf fmt "%s" id
  | LocExpr (LocId id, le) -> fprintf fmt "%s.%a" id pp_local_expr le
  | Send (expr, LocId loc_id) -> fprintf fmt "%a ~> %s" pp_choreo_expr expr loc_id
  | Sync (LocId loc_id1, LabelId label, LocId loc_id2, expr) ->
    fprintf fmt "%s[%s] ~> %s; %a" loc_id1 label loc_id2 pp_choreo_expr expr
  | If (cond, then_expr, else_expr) ->
    fprintf
      fmt
      "@[<v>if %a then@;<1 2>%a@,@[<v 2>else@;%a@]@]"
      pp_choreo_expr
      cond
      pp_choreo_expr
      then_expr
      pp_choreo_expr
      else_expr
  | Let (decl_block, expr) ->
    fprintf
      fmt
      "@[<hov 2>let %a in@]@;<1 2>@[<2>%a@]"
      pp_stmts
      decl_block
      pp_choreo_expr
      expr
  | FunDef (VarId var_id, expr) -> fprintf fmt "fun %s -> %a" var_id pp_choreo_expr expr
  | FunApp (f, arg) -> fprintf fmt "%a %a" pp_choreo_expr f pp_choreo_expr arg
  | Pair (e1, e2) -> fprintf fmt "(%a, %a)" pp_choreo_expr e1 pp_choreo_expr e2
  | Fst e -> fprintf fmt "fst %a" pp_choreo_expr e
  | Snd e -> fprintf fmt "snd %a" pp_choreo_expr e
  | Left e -> fprintf fmt "left %a" pp_choreo_expr e
  | Right e -> fprintf fmt "right %a" pp_choreo_expr e
  | Match (e, cases) ->
    fprintf fmt "@[<hov>match %a with@]@;@[<2>%a@]" pp_choreo_expr e pp_choreo_cases cases

(** [pp_choreo_cases] takes a formatter [fmt] and a list of cases [case_list] and prints the formatted code of the choreo cases

    - A case is a tuple of a choreo pattern and a choreo expression, [(patn, expr)]
    - if the list is empty, do nothing
    - else, print the first case and call [pp_choreo_cases] recursively on the rest of the list
    - Calls helper function [pp_choreo_case] on each case in the list to pretty print *)
and pp_choreo_cases fmt case_list =
  match case_list with
  | [] -> ()
  | case :: cases ->
    pp_choreo_case fmt case;
    pp_choreo_cases fmt cases

(** [pp_choreo_case] takes a formatter [fmt] and a tuple of a choreo case [(patn, expr)] and prints the formatted code of the choreo case

    - Calls [pp_choreo_pattern] to pretty print the choreo pattern
    - Calls [pp_choreo_expr] to pretty print the choreo expression *)
and pp_choreo_case fmt (patn, expr) =
  fprintf fmt "| %a -> %a\n" pp_choreo_pattern patn pp_choreo_expr expr

(* Local pp*)

(** [pp_local_pattern] takes a formatter [fmt] and a local pattern [patn] and prints the formatted code of the local pattern

    - Variants of patterns include Default, Val, Var, Left, Right, and Pair
    - For variants [Left], [Right], and [Pair], it recursively calls [pp_local_pattern] to pretty print the local pattern *)
and pp_local_pattern fmt patn =
  match patn with
  | Default -> fprintf fmt "_"
  | Val v -> fprintf fmt "%s" (string_of_value v)
  | Var (VarId id) -> fprintf fmt "%s" id
  | Left patn -> fprintf fmt "left %a" pp_local_pattern patn
  | Right patn -> fprintf fmt "right %a" pp_local_pattern patn
  | Pair (patn1, patn2) ->
    fprintf fmt "(%a, %a)" pp_local_pattern patn1 pp_local_pattern patn2

(** [pp_local_typ] takes a formatter [fmt] and a local type [loc_typ] and prints the formatted code of the local type

    - Variants of local types include TUnit, TInt, TString, TBool, TProd, and TSum
    - For variants [TProd] and [TSum], it recursively calls [pp_local_typ] to pretty print the local type *)
and pp_local_typ fmt loc_typ =
  match loc_typ with
  | TUnit -> fprintf fmt "unit"
  | TInt -> fprintf fmt "int"
  | TString -> fprintf fmt "string"
  | TBool -> fprintf fmt "bool"
  | TProd (typ1, typ2) -> fprintf fmt "%a * %a" pp_local_typ typ1 pp_local_typ typ2
  | TSum (typ1, typ2) -> fprintf fmt "%a + %a" pp_local_typ typ1 pp_local_typ typ2

(** [pp_local_expr] takes a formatter [fmt] and a local expression [loc_expr] and prints the formatted code of the local expression

    - Variants of local expressions include Unit, Val, Var, Fst, Snd, Left, Right, Pair, BinOp, Let, and Match
    - For variants [Fst], [Snd], [Left], [Right], [Pair], [BinOp], [Let], [Match] it recursively calls
      [pp_local_expr] to pretty print the local expression
    - For variant [BinOp], it also calls helper function [pp_bin_op] to pretty print the binop
    - For variant [Match], it also calls helper function [pp_local_cases] to pretty print the local cases *)
and pp_local_expr fmt loc_expr =
  match loc_expr with
  | Unit -> fprintf fmt "()"
  | Val ((`Int _ | `String _ | `Bool _) as v) -> fprintf fmt "%s" (string_of_value v)
  | Var (VarId id) -> fprintf fmt "%s" id
  | Fst e -> fprintf fmt "fst %a" pp_local_expr e
  | Snd e -> fprintf fmt "snd %a" pp_local_expr e
  | Left le -> fprintf fmt "left %a" pp_local_expr le
  | Right le -> fprintf fmt "right %a" pp_local_expr le
  | Pair (le1, le2) -> fprintf fmt "(%a, %a)" pp_local_expr le1 pp_local_expr le2
  | BinOp (e1, op, e2) ->
    fprintf fmt "%a %a %a" pp_local_expr e1 pp_bin_op op pp_local_expr e2
  | Let (VarId id, e1, e2) ->
    fprintf
      fmt
      "@[<hov 2>let %s := %a in@]@;<1 2>@[<2>%a@]"
      id
      pp_local_expr
      e1
      pp_local_expr
      e2
  | Match (e, cases) ->
    fprintf fmt "@[<hov>match %a with@]@;@[<2>%a@]" pp_local_expr e pp_local_cases cases

(** [pp_local_cases] takes a formatter [fmt] and a list of cases [case_list] and prints the formatted code of the local cases

    - A case is a tuple of a local pattern and a local expression, [(patn, expr)]
    - if the list is empty, do nothing
    - else, print the first case and call [pp_local_cases] recursively on the rest of the list
    - Calls helper function [pp_local_case] on each local case in the list to pretty print *)
and pp_local_cases fmt case_list =
  match case_list with
  | [] -> ()
  | case :: cases ->
    pp_local_case fmt case;
    pp_local_cases fmt cases

(** [pp_local_case] takes a formatter [fmt] and a tuple of a local case [(patn, expr)] and prints the formatted code of the local case

    - Calls [pp_local_pattern] to pretty print the local pattern
    - Calls [pp_local_expr] to pretty print the local expression *)
and pp_local_case fmt (patn, expr) =
  fprintf fmt "| %a -> %a\n" pp_local_pattern patn pp_local_expr expr

(** [pp_bin_op] takes a formatter [fmt] and a binary operator [op] and prints the formatted code of the binary operator

    - Variants of binary operator includes Plus, Minus, Times, Div, And, Or, Eq, Neq, Lt, Leq, Gt, or Geq.
    - The formatted code for the binary operator is a string representation of the operator. *)
and pp_bin_op fmt op =
  match op with
  | Plus -> fprintf fmt "+"
  | Minus -> fprintf fmt "-"
  | Times -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"
  | Eq -> fprintf fmt "="
  | Neq -> fprintf fmt "!="
  | Lt -> fprintf fmt "<"
  | Leq -> fprintf fmt "<="
  | Gt -> fprintf fmt ">"
  | Geq -> fprintf fmt ">="

(** [string_of_value v] returns a string representation of the value [v]

    - [v] is a value that is either an integer, string, or boolean.
    - Returns: A string representation of the value [v]. *)
and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> "\"" ^ s ^ "\""
  | `Bool b -> string_of_bool b
;;
