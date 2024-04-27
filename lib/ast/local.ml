type filename = string
type line = int
type range = line * line
type metainfo = filename * line (* define metainfo, filename, and line number *)

type value =
  [ `Int of int
  | `String of string
  | `Bool of bool ]

type loc_id = LocId of string * metainfo
type var_id = VarId of string * metainfo
type sync_label = LabelId of string * metainfo



(*
  Type: bin_op
  Description: Represents binary operations in the AST with associated metadata(see above).

  Variants:
  - Eq: Represents equality (==)
  - Neq: Represents inequality (!=)
  - Lt: Represents less than (<)
  - Leq: Represents less than or equal (<=)
  - Gt: Represents greater than (>)
  - Geq: Represents greater than or equal (>=)

  Each variant is tagged with a `metainfo` (see above).
*)

type bin_op =
  | Plus of metainfo
  | Minus of metainfo
  | Times of metainfo
  | Div of metainfo
  | And of metainfo
  | Or of metainfo
  | Eq of metainfo
  | Neq of metainfo
  | Lt of metainfo
  | Leq of metainfo
  | Gt of metainfo
  | Geq of metainfo

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
