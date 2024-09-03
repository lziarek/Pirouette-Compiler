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

open Local
open Format

(** [pp_local_pattern] takes a formatter [fmt] and a local pattern [patn] and prints the formatted code of the local pattern

    - Variants of patterns include Default, Val, Var, Left, Right, and Pair
    - For variants [Left], [Right], and [Pair], it recursively calls [pp_local_pattern] to pretty print the local pattern
*)
let rec pp_local_pattern fmt patn =
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
    - For variants [TProd] and [TSum], it recursively calls [pp_local_typ] to pretty print the local type
*)
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

(** [pp_local_expr] takes a formatter [fmt] and a local expression [loc_expr] and prints the formatted code of the local expression

    - Variants of local expressions include Unit, Val, Var, Fst, Snd, Left, Right, Pair, BinOp, Let, and Match
    - For variants [Fst], [Snd], [Left], [Right], [Pair], [BinOp], [Let], [Match] it recursively calls 
      [pp_local_expr] to pretty print the local expression
    - For variant [BinOp], it also calls helper function [pp_bin_op] to pretty print the binop
    - For variant [Match], it also calls helper function [pp_local_cases] to pretty print the local cases
*)
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
      fprintf fmt "@[<hov 2>let %s := %a in@]@;<1 2>@[<2>%a@]" id pp_local_expr e1 pp_local_expr e2
  | Match (e, cases) ->
      fprintf fmt "@[<hov>match %a with@]@;@[<2>%a@]" pp_local_expr e pp_local_cases cases

(** [pp_local_cases] takes a formatter [fmt] and a list of cases [case_list] and prints the formatted code of the local cases

    - A case is a tuple of a local pattern and a local expression, [(patn, expr)]
    - if the list is empty, do nothing
    - else, print the first case and call [pp_local_cases] recursively on the rest of the list
    - Calls helper function [pp_local_case] on each local case in the list to pretty print
*)
and pp_local_cases fmt case_list = 
  match case_list with
  | [] -> ()
  | case :: cases -> 
    pp_local_case fmt case;
    pp_local_cases fmt cases

(** [pp_local_case] takes a formatter [fmt] and a tuple of a local case [(patn, expr)] and prints the formatted code of the local case

    - Calls [pp_local_pattern] to pretty print the local pattern
    - Calls [pp_local_expr] to pretty print the local expression
*)
and pp_local_case fmt (patn, expr) =
  fprintf fmt "| %a -> %a\n" pp_local_pattern patn pp_local_expr expr

(** [pp_bin_op] takes a formatter [fmt] and a binary operator [op] and prints the formatted code of the binary operator

    - Variants of binary operator includes Plus, Minus, Times, Div, And, Or, Eq, Neq, Lt, Leq, Gt, or Geq.
    - The formatted code for the binary operator is a string representation of the operator.
*)
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
    - Returns: A string representation of the value [v].
*)
and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> "\"" ^ s ^ "\""
  | `Bool b -> string_of_bool b