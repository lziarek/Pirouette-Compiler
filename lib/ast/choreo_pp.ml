(*
  Files: choreo_pp.ml & local_pp.ml
  Date: 04/25/2024

  A pretty print library to print Pirouette code in format using
  the Format module in OCaml: https://v2.ocaml.org/api/Format.html

  Format.formatter fmt: 
  print code to:
  - standard output Stdlib.stdout: Format.std_formatter
  - standard error Stdlib.stderr: Format.err_formatter
  - string {use stdbuffer in Format lib}: Format.str_formatter & Format.flush_str_formatter
  - file: Format.formatter_of_out_channel (open_out "file_name")
*)

(*
  pp_ast:
  Input: 
    - Format.formatter fmt
    - Ast.Choreo.Prog stmts: Choreo AST

  Print the list of statements in the format and a new line when finish
*)

open Choreo
open Format
open Local_pp

(** [pp_ast] prints the list of statements in format, then a new line when finished
    
    - [fmt] is Format.formatter
    - [stmts] is a Choreo AST of type Choreo.Prog
    - Calls helper function [pp_stmts] to pretty print the Choreo AST
    - Calls a formatter function [pp_print_newline] to print all pending items in the pretty-printer queue, 
      flush the output device, and add a new line.
*)
let rec pp_ast fmt (Prog stmts) = 
  pp_stmts fmt stmts;
  pp_print_newline fmt ()

(*
  pp_stmts:
  Input: 
    - Format.formatter fmt
    - Ast.Choreo.decl_block stmts: a list of statements

  use pattern matching to print each statement in the list
  if the list is empty, do nothing
  else print the first statement and call pp_stmts recursively on the rest of the list
*)
(** [pp_stmts] uses pattern matching to print each statements in the list
  
    - if the list is empty, do nothing
    - else, print the first statement and call pp_stmts recursively on the rest of the list
    - [fmt] is Format.formatter
    - [stmts] is a list of statements of type Choreo.decl_block
    - Calls helper function [pp_stmt] on each statement in [stmts] to pretty print
*)
and pp_stmts fmt stmts = 
  match stmts with
  | [] -> ()
  | stmt :: stmts -> 
    pp_stmt fmt stmt;
    if stmts <> [] then fprintf fmt "\n";
    pp_stmts fmt stmts

(** [pp_stmt] takes a formatter [fmt] and a statement [statement] and prints the formatted code of the statement
  
    - Variants of statements include Decl, Assign, TypeDecl
    - Calls helper functions [pp_choreo_pattern], [pp_choreo_typ], [pp_choreo_expr] to pretty print the statement
*)
and pp_stmt fmt statement =
  match statement with
  | Decl (patn, typ) ->
      fprintf fmt "%a : %a;\n" pp_choreo_pattern patn pp_choreo_typ typ
  | Assign (patn, expr) ->
    fprintf fmt "%a :=@;<1 2>%a;" pp_choreo_pattern patn pp_choreo_expr expr
  | TypeDecl (VarId id, typ) ->
      fprintf fmt "type %s := %a;\n" id pp_choreo_typ typ

(** [pp_choreo_pattern] takes a formatter [fmt] and a choreo pattern [patn] and prints the formatted code of the choreo pattern

    - Variants of patterns include Default, LocPatt, Var, Left, Right, and Pair
    - For variant [LocPatt], it calls helper function [pp_local_pattern] to pretty print the local pattern
    - For variants [Pair], [Left], and [Right], it calls helper function [pp_choreo_pattern] to pretty print the choreo pattern
*)
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
    - For variants [TSend], [TProd], and [TSum], it calls helper function [pp_choreo_typ] to pretty print the choreo type
*)
and pp_choreo_typ fmt choreo_typ =
  match choreo_typ with
  | TUnit -> fprintf fmt "unit"
  | TLoc (LocId id, typ) -> fprintf fmt "%s.(%a)" id pp_local_typ typ
  | TSend (typ1, typ2) ->
      fprintf fmt "%a -> %a" pp_choreo_typ typ1 pp_choreo_typ typ2
  | TProd (typ1, typ2) ->
      fprintf fmt "%a * %a" pp_choreo_typ typ1 pp_choreo_typ typ2
  | TSum (typ1, typ2) ->
        fprintf fmt "%a + %a" pp_choreo_typ typ1 pp_choreo_typ typ2

(** [pp_choreo_expr] takes a formatter [fmt] and a choreo expression [expr] and prints the formatted code of the choreo expression

    - Variants of choreo expressions include Unit, Var, LocExpr, Send, Sync, If, Let, FunDef, FunApp,
      Pair, Fst, Snd, Left, Right, and Match
    - For variant [LocExpr], it calls helper function [pp_local_expr] to pretty print the local expression
    - For variant [Let], it calls helper function [pp_stmts] to pretty print the declaration block 
    - For variant [Match], it calls helper function [pp_choreo_cases] to pretty print the choreo cases
*)
and pp_choreo_expr fmt expr =
  match expr with
  | Unit -> fprintf fmt "()"
  | Var (VarId id) -> fprintf fmt "%s" id
  | LocExpr (LocId id, le) -> fprintf fmt "%s.%a" id pp_local_expr le
  | Send (expr, LocId loc_id) ->
    fprintf fmt "%a ~> %s" pp_choreo_expr expr loc_id
  | Sync (LocId loc_id1, LabelId label, LocId loc_id2, expr) ->
    fprintf fmt "%s[%s] ~> %s; %a" loc_id1 label loc_id2 pp_choreo_expr expr
  | If (cond, then_expr, else_expr) ->
    fprintf fmt "@[<v>if %a then@;<1 2>%a@,@[<v 2>else@;%a@]@]" pp_choreo_expr cond pp_choreo_expr then_expr pp_choreo_expr else_expr
  | Let (decl_block, expr) ->
    fprintf fmt "@[<hov 2>let %a in@]@;<1 2>@[<2>%a@]" pp_stmts decl_block pp_choreo_expr expr
  | FunDef (VarId var_id, expr) ->
    fprintf fmt "fun %s -> %a" var_id pp_choreo_expr expr
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
    - Calls helper function [pp_choreo_case] on each case in the list to pretty print
*)
and pp_choreo_cases fmt case_list = 
  match case_list with
  | [] -> ()
  | case :: cases -> 
    pp_choreo_case fmt case;
    pp_choreo_cases fmt cases

(** [pp_choreo_case] takes a formatter [fmt] and a tuple of a choreo case [(patn, expr)] and prints the formatted code of the choreo case

    - Calls [pp_choreo_pattern] to pretty print the choreo pattern
    - Calls [pp_choreo_expr] to pretty print the choreo expression
*)
and pp_choreo_case fmt (patn, expr) =
  fprintf fmt "| %a -> %a\n" pp_choreo_pattern patn pp_choreo_expr expr
