(*
   File: pprint_ast.ml
  Date: 09/18/2024

  A pretty print library to print Pirouette code in format using
  the Format module in OCaml: https://v2.ocaml.org/api/Format.html

  Format.formatter fmt: 
  print code to:
  - standard output Stdlib.stdout: Format.std_formatter
  - standard error Stdlib.stderr: Format.err_formatter
  - string {use stdbuffer in Format lib}: Format.str_formatter & Format.flush_str_formatter
  - file: Format.formatter_of_out_channel (open_out "file_name")
*)

module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M
open Format

(* ============================== Local ============================== *)

(** [pprint_local_type] takes a formatter [ppf] and a local type,
    and prints the formatted code of the local type

    - Variants of local types include TUnit, TInt, TString, TBool, TProd, and TSum
    - For variants [TProd] and [TSum], it recursively calls
      [pprint_local_type] to pretty print the local type *)
let rec pprint_local_type ppf (typ : 'a Local.typ) =
  match typ with
  | TUnit _ -> fprintf ppf "@[<h>unit@]"
  | TInt _ -> fprintf ppf "@[<h>int@]"
  | TString _ -> fprintf ppf "@[<h>string@]"
  | TBool _ -> fprintf ppf "@[<h>bool@]"
  | TVar (TypId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | TProd (t1, t2, _) ->
    fprintf ppf "@[<h>%a * %a@]" pprint_local_type t1 pprint_local_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>%a + %a@]" pprint_local_type t1 pprint_local_type t2
;;

(** [pprint_local_pattern] takes a formatter [ppf] and a local pattern,
    and prints the formatted code of the local pattern

    - Variants of patterns include Default, Val, Var, Left, Right, and Pair
    - For variants [Left], [Right], and [Pair], it recursively calls
      [pprint_local_pattern] to pretty print the local pattern *)
let rec pprint_local_pattern ppf (pat : 'a Local.pattern) =
  match pat with
  | Default _ -> fprintf ppf "@[<h>_@]"
  | Val (v, _) ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf (v : 'a Local.value) ->
        match v with
        | Int (i, _) -> fprintf ppf "%d" i
        | String (s, _) -> fprintf ppf "\"%s\"" s
        | Bool (b, _) -> fprintf ppf "%b" b)
      v
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Pair (p1, p2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_local_pattern p1 pprint_local_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_local_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_local_pattern p
;;

(** [pprint_local_expr] takes a formatter [ppf] and a local expression, and prints the formatted code of the local expression

    - For variants [Fst], [Snd], [Left], [Right], [Pair], [BinOp],[UnOp] [Let], [Match]
      it recursively calls [pprint_local_expr] to pretty print the local expression
    - For variant [Match], it also calls helper function [pprint_local_case]
      and [pp_print_list] provided by Format module to pretty print the list of local case *)
let rec pprint_local_expr ppf (expr : 'a Local.expr) =
  match expr with
  | Unit _ -> fprintf ppf "@[<h>()@]"
  | Val (v, _) ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf (v : 'a Local.value) ->
        match v with
        | Int (i, _) -> fprintf ppf "%d" i
        | String (s, _) -> fprintf ppf "\"%s\"" s
        | Bool (b, _) -> fprintf ppf "%b" b)
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
  | Let (VarId (id, _), typ, e1, e2, _) ->
    fprintf
      ppf
      "@[<v2>let@ %s : %a := %a@;<1 -2>in@ %a@]"
      id
      pprint_local_type
      typ
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
    let[@inline] pprint_local_case ppf (p, e) =
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_local_expr e
    in
    fprintf
      ppf
      "@[<v>match %a with@ | %a@]"
      pprint_local_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_local_case)
      cases
;;

(* ============================== Choreo ============================== *)

(** [pp_choreo_typ] takes a formatter [fmt] and a choreo type,
    and prints the formatted code of the choreo type

    - For variant [TLoc], it calls helper function [pprint_local_type]
      to pretty print the local type
    - For variants [TSend], [TProd], [TMap] and [TSum], it calls helper function
      [pprint_choreo_type] to pretty print the choreo type *)
let rec pprint_choreo_type ppf (typ : 'a Choreo.typ) =
  match typ with
  | TUnit _ -> fprintf ppf "@[<h>unit@]"
  | TLoc (LocId (loc, _), t, _) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_type t
  | TMap (t1, t2, _) ->
    fprintf ppf "@[<h>%a ->@ %a@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TProd (t1, t2, _) ->
    fprintf ppf "@[<h>%a *@ %a@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
;;

(** [pp_choreo_pattern] takes a formatter [fmt] and a choreo pattern,
    and prints the formatted code of the choreo pattern

    - For variant [LocPat], it calls helper function [pprint_local_pattern]
      to pretty print the local pattern
    - For variants [Pair], [Left], and [Right], it calls helper function
      [pprint_choreo_pattern] to pretty print the choreo pattern *)
let rec pprint_choreo_pattern ppf (pat : 'a Choreo.pattern) =
  match pat with
  | Default _ -> fprintf ppf "@[<h>_@]"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | LocPat (LocId (loc, _), p, _) -> fprintf ppf "@[<h>%s.%a@]" loc pprint_local_pattern p
  | Pair (p1, p2, _) ->
    fprintf ppf "@[<hv>(%a, %a)@]" pprint_choreo_pattern p1 pprint_choreo_pattern p2
  | Left (p, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_choreo_pattern p
  | Right (p, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_choreo_pattern p
;;

(** [pprint_choreo_stmt_block] takes a formatter [ppf] and a choreo statement block,
    and prints the formatted code of the choreo statement block

    - It uses [pp_print_list] provided by Format module to
      pretty print the list of choreo statements *)
let[@specialise] rec pprint_choreo_stmt_block ppf (stmts : 'a Choreo.stmt_block) =
  fprintf ppf "@[<v>@[<v>%a@]@]" (pp_print_list pprint_choreo_stmt) stmts

(** [pprint_choreo_stmt] takes a formatter [ppf] and a statement,
    and prints the formatted code of the statement

    - Variants of statements include Decl, Assign, TypeDecl
    - Calls helper functions [pprint_choreo_pattern], [pprint_choreo_type],
      [pprint_choreo_expr] to pretty print the statement *)
and pprint_choreo_stmt ppf (stmt : 'a Choreo.stmt) =
  match stmt with
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
  | TypeDecl (TypId (id, _), t, _) ->
    fprintf ppf "@[<h>type %s := %a;@]" id pprint_choreo_type t

(** [pp_choreo_expr] takes a formatter [ppf] and a choreo expression
    and prints the formatted code of the choreo expression

    - For variant [LocExpr], it calls helper function [pprint_local_expr] to pretty print the local expression
    - For variant [Let], it calls helper function [pprint_choreo_stmt_block] to pretty print the declaration block
    - For variant [Match], it calls helper function [pprint_choreo_case]
      and [pp_print_list] provided by Format module to pretty print the list of cases *)
and pprint_choreo_expr ppf (expr : 'a Choreo.expr) =
  match expr with
  | Unit _ -> fprintf ppf "@[<h>()@]"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst@ %a@]" pprint_choreo_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd@ %a@]" pprint_choreo_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left@ %a@]" pprint_choreo_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right@ %a@]" pprint_choreo_expr e
  | LocExpr (LocId (loc, _), e, _) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_expr e
  | Send (LocId (loc1, _), e, LocId (loc2, _), _) ->
    fprintf ppf "@[<hv2>[%s] %a@ ~> %s@]" loc1 pprint_choreo_expr e loc2
  | Sync (LocId (loc1, _), LabelId (label, _), LocId (loc2, _), e, _) ->
    fprintf ppf "@[<hv>%s[%s] ~> %s;@ %a@]" loc1 label loc2 pprint_choreo_expr e
  | If (e1, e2, e3, _) ->
    fprintf
      ppf
      "@[<v>if@;<1 2>%a@ then@;<1 2>%a@ else@;<1 2>%a@]"
      pprint_choreo_expr
      e1
      pprint_choreo_expr
      e2
      pprint_choreo_expr
      e3
  | Let (stmt_block, e, _) ->
    fprintf
      ppf
      "@[<v2>let@ %a@;<1 -2>in@ %a@]"
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
    let[@inline] pprint_choreo_case ppf (p, e) =
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_choreo_pattern p pprint_choreo_expr e
    in
    fprintf
      ppf
      "@[<v>match %a with@ | %a@]"
      pprint_choreo_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_choreo_case)
      cases
;;

(* ============================== Net ============================== *)

let rec pprint_net_type ppf (typ : 'a Net.typ) =
  match typ with
  | TUnit _ -> fprintf ppf "@[<h>unit@]"
  | TLoc (t, _) -> fprintf ppf "@[<h>%a@]" pprint_local_type t
  | TMap (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) -> (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TProd (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TSum (t1, t2, _) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_net_type t1 pprint_net_type t2
;;

let[@specialise] rec pprint_net_stmt_block ppf (stmts : 'a Net.stmt_block) =
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]" (pp_print_list pprint_net_stmt) stmts

and pprint_net_stmt ppf (stmt : 'a Net.stmt) =
  match stmt with
  | Decl (p, t, _) ->
    fprintf ppf "@[<h>(%a) : %a@]" pprint_local_pattern p pprint_net_type t
  | Assign (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>(%a) :=@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | TypeDecl (TypId (id, _), t, _) -> fprintf ppf "@[<h>%s : %a@]" id pprint_net_type t

and pprint_net_expr ppf (expr : 'a Net.expr) =
  match expr with
  | Unit _ -> fprintf ppf "@[<h>()@]"
  | Var (VarId (id, _), _) -> fprintf ppf "@[<h>%s@]" id
  | Ret (e, _) -> fprintf ppf "@[<hv2>ret@ (%a)@]" pprint_local_expr e
  | Fst (e, _) -> fprintf ppf "@[<hv2>fst@ (%a)@]" pprint_net_expr e
  | Snd (e, _) -> fprintf ppf "@[<hv2>snd@ (%a)@]" pprint_net_expr e
  | Left (e, _) -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_net_expr e
  | Right (e, _) -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_net_expr e
  | Send (e, LocId (loc, _), _) -> fprintf ppf "@[<hv2>(%a) ~> %s@]" pprint_net_expr e loc
  | Recv (LocId (loc, _), _) -> fprintf ppf "@[<~ %s@]" loc
  | If (e1, e2, e3, _) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
      pprint_net_expr
      e1
      pprint_net_expr
      e2
      pprint_net_expr
      e3
  | Let (stmt_block, e, _) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ (%a)@]"
      pprint_net_stmt_block
      stmt_block
      pprint_net_expr
      e
  | FunDef (ps, e, _) ->
    fprintf
      ppf
      "@[<hv2>fun@ (%a) ->@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | FunApp (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a)@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Pair (e1, e2, _) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Match (e, cases, _) ->
    let[@inline] pprint_net_case ppf (p, e) =
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_net_expr e
    in
    fprintf
      ppf
      "@[<hv>match (%a)@ with@[<v2>@ | %a@]@]"
      pprint_net_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_case)
      cases
  | ChooseFor (LabelId (id, _), LocId (locid, _), e, _) ->
    fprintf ppf "@[<hv>choose %s for %s;@ %a@]" id locid pprint_net_expr e
  | AllowChoice (LocId (loc, _), choices, _) ->
    let[@inline] pprint_net_choice ppf (Local.LabelId (id, _), e) =
      fprintf ppf "@[<hv2>%s ->@ %a@]" id pprint_net_expr e
    in
    fprintf
      ppf
      "@[<v>allow %s choice@ | %a@]"
      loc
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_choice)
      choices
;;
