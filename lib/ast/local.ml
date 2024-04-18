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
