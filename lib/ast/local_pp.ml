open Local
open Format

let rec pp_local_pattern fmt patn =
  match patn with
  | Default -> fprintf fmt "Default"
  | Val v -> fprintf fmt "%s" (string_of_value v)
  | Var (VarId id) -> fprintf fmt "%s" id
  | Left patn -> fprintf fmt "left %a" pp_local_pattern patn
  | Right patn -> fprintf fmt "right %a" pp_local_pattern patn
  | Pair (patn1, patn2) ->
      fprintf fmt "(%a, %a)" pp_local_pattern patn1 pp_local_pattern patn2

and pp_local_typ fmt loc_typ =
  match loc_typ with
  | TUnit -> fprintf fmt "unit"
  | TInt -> fprintf fmt "int"
  | TString -> fprintf fmt "string"
  | TBool -> fprintf fmt "bool"
  | TProd (typ1, typ2) ->
      fprintf fmt "%a * %a" pp_local_typ typ1 pp_local_typ typ2
  | TSum (typ1, typ2) ->
      fprintf fmt "%a + %a" pp_local_typ typ1 pp_local_typ typ2

and pp_local_expr fmt loc_expr =
  match loc_expr with
  | Unit -> fprintf fmt "()"
  | Val ((`Int _ | `String _ | `Bool _) as v) ->
      fprintf fmt "%s" (string_of_value v)
  | Var (VarId id) -> fprintf fmt "%s" id
  | Fst e -> fprintf fmt "fst %a" pp_local_expr e
  | Snd e -> fprintf fmt "snd %a" pp_local_expr e
  | Left le -> fprintf fmt "left %a" pp_local_expr le
  | Right le -> fprintf fmt "right %a" pp_local_expr le
  | Pair (le1, le2) ->
      fprintf fmt "(%a, %a)" pp_local_expr le1 pp_local_expr le2
  | BinOp (e1, op, e2) ->
      fprintf fmt "%a %a %a" pp_local_expr e1 pp_bin_op op pp_local_expr e2
  | Let (VarId id, e1, e2) ->
      fprintf fmt "let %s := %a in@ %a" id pp_local_expr e1 pp_local_expr e2
  | Match (e, cases) ->
      fprintf fmt "@[<v>match %a with@;<1 2>%a@]" pp_local_expr e pp_local_cases cases

and pp_local_cases fmt case_list = 
  match case_list with
  | [] -> ()
  | case :: cases -> 
    pp_local_case fmt case;
    pp_local_cases fmt cases

and pp_local_case fmt (patn, expr) =
  fprintf fmt "| %a -> %a@" pp_local_pattern patn pp_local_expr expr

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

and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> s
  | `Bool b -> string_of_bool b
