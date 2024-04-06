type value =
  [ `Int of int
  | `String of string
  | `Bool of bool ]

type loc_id = LocId of string
type var_id = VarId of string
type sync_label = LabelId of string

type bin_op =
  | Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

type local_type =
  | TUnit
  | TInt
  | TString
  | TBool
  | TProd of local_type * local_type
  | TSum of local_type * local_type

type local_pattern =
  | Default
  | Val of value
  | Var of var_id
  | Pair of local_pattern * local_pattern
  | Left of local_pattern
  | Right of local_pattern

type local_expr =
  | Unit
  | Val of value
  | Var of var_id
  | BinOp of local_expr * bin_op * local_expr
  | Let of var_id * local_expr * local_expr
  | Pair of local_expr * local_expr
  | Fst of local_expr
  | Snd of local_expr
  | Left of local_expr
  | Right of local_expr
  | Match of local_expr * (local_pattern * local_expr) list

let rec dot_local_expr loc_expr =
  match loc_expr with
  | Unit -> print_endline "Unit"
  | Val ((`Int _ | `String _ | `Bool _) as v) ->
      print_endline (string_of_value v)
  | Var (VarId id) -> print_endline id
  | BinOp (e1, op, e2) ->
      dot_local_expr e1;
      dot_bin_op op;
      dot_local_expr e2
  | Let (VarId id, e1, e2) ->
      print_endline ("Let: " ^ id);
      dot_local_expr e1;
      dot_local_expr e2
  | Pair (e1, e2) ->
      print_endline "Pair";
      dot_local_expr e1;
      dot_local_expr e2
  | Fst e ->
      print_endline "Fst";
      dot_local_expr e
  | Snd e ->
      print_endline "Snd";
      dot_local_expr e
  | Left e ->
      print_endline "Left";
      dot_local_expr e
  | Right e ->
      print_endline "Right";
      dot_local_expr e
  | Match (e, cases) ->
      print_endline "Match";
      dot_local_expr e;
      dot_local_cases cases

and dot_local_pattern patn =
  match patn with
  | Default -> print_endline "Default"
  | Val v -> print_endline (string_of_value v)
  | Var (VarId id) -> print_endline id
  | Pair (patn1, patn2) ->
      print_endline "Pair";
      dot_local_pattern patn1;
      dot_local_pattern patn2
  | Left patn ->
      print_endline "Left";
      dot_local_pattern patn
  | Right patn ->
      print_endline "Right";
      dot_local_pattern patn

and dot_local_type typ =
  match typ with
  | TUnit -> print_endline "Unit"
  | TInt -> print_endline "Int"
  | TString -> print_endline "String"
  | TBool -> print_endline "Bool"
  | TProd (typ1, typ2) ->
      print_endline "Product";
      dot_local_type typ1;
      dot_local_type typ2
  | TSum (typ1, typ2) ->
      print_endline "Sum";
      dot_local_type typ1;
      dot_local_type typ2

and dot_bin_op op =
  match op with
  | Plus -> print_endline "+"
  | Minus -> print_endline "-"
  | Times -> print_endline "*"
  | Div -> print_endline "/"
  | And -> print_endline "&&"
  | Or -> print_endline "||"
  | Eq -> print_endline "="
  | Neq -> print_endline "!="
  | Lt -> print_endline "<"
  | Leq -> print_endline "<="
  | Gt -> print_endline ">"
  | Geq -> print_endline ">="

and string_of_value v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> s
  | `Bool b -> string_of_bool b

and dot_local_cases cases = List.iter dot_local_case cases

and dot_local_case (patn, expr) = 
  print_endline "Case"
  dot_local_pattern patn;
  dot_local_expr expr