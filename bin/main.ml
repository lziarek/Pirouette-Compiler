(* open Ast.Interface *)
open Parsing.Interface
open Format

let rec pp_ast fmt (Ast.Choreo.Prog stmts) =
    List.iter (pp_stmt fmt) stmts

and pp_stmt fmt (statement) =
    match statement with
    | Decl (patn, typ) -> fprintf fmt "%a :@ %a ;@" pp_pattern patn pp_choreo_typ typ
    | Assign (patn, expr) -> fprintf fmt "%a :=@ %a ;@" pp_pattern patn pp_choreo_expr expr
    | TypeDecl (VarId id, typ) -> fprintf fmt "type@ `%s := @ %a ;@" id pp_choreo_typ typ

and pp_pattern fmt (patn) =
    match patn with
    | Default -> fprintf fmt "Default"
    | LocPatt (LocId loc, lp) -> fprintf fmt "%s .@ %a" loc pp_local_pattern lp
    (*not for local AST, need to be modified if use*)
    | Var (VarId id) -> fprintf fmt "Var@ `%s" id
    | _ -> fprintf fmt "Not implemented"

and pp_local_pattern fmt (patn) =
    match patn with
    | Default -> fprintf fmt "Default"
    | Val (v) -> fprintf fmt "%s" (string_of_value v)
    | Var (VarId id) -> fprintf fmt "%s" id
    | Left patn -> fprintf fmt "left@ %a" pp_local_pattern patn
    | Right patn -> fprintf fmt "right@ %a" pp_local_pattern patn
    | Pair (patn1, patn2) -> fprintf fmt "(@ %a ,@ %a )@" pp_local_pattern patn1 pp_local_pattern patn2

and pp_choreo_typ fmt (choreo_typ) =
    match choreo_typ with
    | TUnit -> fprintf fmt "unit"
    | TLoc (LocId id, typ) -> fprintf fmt "%s .@ %a" id pp_local_typ typ
    (* not for local AST, need to be modified if use*)
    | _ -> fprintf fmt "Not implemented"

and pp_local_typ fmt (loc_typ) =
    match loc_typ with
    | TUnit -> fprintf fmt "unit"
    | TInt -> fprintf fmt "int"
    | TString -> fprintf fmt "string"
    | TBool -> fprintf fmt "bool"
    | TProd (typ1, typ2) -> fprintf fmt "%a *@ %a" pp_local_typ typ1 pp_local_typ typ2
    | TSum (typ1, typ2) -> fprintf fmt "%a +@ %a" pp_local_typ typ1 pp_local_typ typ2

and pp_choreo_expr fmt (expr) =
    match expr with
    | Unit -> fprintf fmt "()"
    | Var (VarId id) -> fprintf fmt "%s" id
    | LocExpr (LocId id, le) -> fprintf fmt "%s.@[%a@]" id pp_local_expr le
    | Send (expr, LocId loc_id) -> fprintf fmt "%a ~> %s" pp_choreo_expr expr loc_id
    | Sync (LocId loc_id1, LabelId label, LocId loc_id2, expr) ->
        fprintf fmt "%s[%s] ~> %s;@ %a" loc_id1 label loc_id2 pp_choreo_expr expr
    | If (cond, then_expr, else_expr) ->
        fprintf fmt "if %a then %a else %a" pp_choreo_expr cond pp_choreo_expr then_expr pp_choreo_expr else_expr
    | Let (decl_block, expr) ->  (* Adjusted to include decl_block *)
        fprintf fmt "let ";
        List.iter (fun stmt -> pp_stmt fmt stmt; fprintf fmt " ") decl_block;
        fprintf fmt "in %a" pp_choreo_expr expr
    | FunDef (VarId var_id, expr) ->
        fprintf fmt "fun %s -> %a" var_id pp_choreo_expr expr
    | FunApp (f, arg) ->
        fprintf fmt "%a %a" pp_choreo_expr f pp_choreo_expr arg
    | Pair (e1, e2) ->
        fprintf fmt "(%a, %a)" pp_choreo_expr e1 pp_choreo_expr e2
    | Fst e ->
        fprintf fmt "fst %a" pp_choreo_expr e
    | Snd e ->
        fprintf fmt "snd %a" pp_choreo_expr e
    | Left e ->
        fprintf fmt "left %a" pp_choreo_expr e
    | Right e ->
        fprintf fmt "right %a" pp_choreo_expr e
    | Match (e, _) ->  (* Adjusted to ignore cases *)
        fprintf fmt "match %a with ... " pp_choreo_expr e  (* Simplified for brevity *)

and pp_local_expr fmt (loc_expr) =
    match loc_expr with
    | Unit -> fprintf fmt "()"
    | Val (`Int _ | `String _ | `Bool _ as v) -> fprintf fmt "%s" (string_of_value v)
    | Var (VarId id) -> fprintf fmt "%s" id
    | Fst e -> fprintf fmt "fst@ %a" pp_local_expr e
    | Snd e -> fprintf fmt "snd@ %a" pp_local_expr e
    | Left le -> fprintf fmt "left@ %a" pp_local_expr le
    | Right le -> fprintf fmt "right@ %a" pp_local_expr le
    | Pair (le1, le2) -> fprintf fmt "(@ %a ,@ %a )@" pp_local_expr le1 pp_local_expr le2
    | BinOp (e1, op, e2) -> fprintf fmt "%a %a %a" pp_local_expr e1 pp_bin_op op pp_local_expr e2
    | Let (VarId id, e1, e2) -> fprintf fmt "let@ %s :=@ %a in@ %a" id pp_local_expr e1 pp_local_expr e2
    | Match (e, cases) -> fprintf fmt "match@ %a with@ %a" pp_local_expr e pp_local_cases cases

and pp_bin_op fmt (op) =
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

and string_of_value (v) =
    match v with
    | `Int i -> string_of_int i
    | `String s -> s
    | `Bool b -> string_of_bool b

and pp_local_cases fmt (case_list) =
    List.iter (pp_local_case fmt) case_list

and pp_local_case fmt (patn,expr) =
    fprintf fmt "|@ %a ->@ %a @\n" pp_local_pattern patn pp_local_expr expr


let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: <executable> <sourcefile>";
    exit 1)
  else
    let filename = Sys.argv.(1) in
    let file_ic = open_in filename in
    let lexbuf = Lexing.from_channel file_ic in
    let program = parse_program lexbuf in
        (*print_endline (dump_choreo_ast program)*)
        pp_ast Format.std_formatter program;
        Format.pp_print_flush Format.std_formatter ();
    close_in file_ic
