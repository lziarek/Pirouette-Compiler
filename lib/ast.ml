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

type choreo_type =
  | TUnit
  | TLoc of loc_id * local_type
  | TSend of choreo_type * choreo_type
  | TProd of choreo_type * choreo_type
  | TSum of choreo_type * choreo_type

type network_type =
  | TUnit
  | TSend of network_type * network_type
  | TProd of network_type * network_type
  | TSum of network_type * network_type

type local_pattern =
  | Default
  | Val of value
  | Var of var_id
  | Pair of local_pattern * local_pattern
  | Left of local_pattern
  | Right of local_pattern

type pattern =
  | Default
  | Var of var_id
  | Pair of pattern * pattern
  | LocPatt of loc_id * local_pattern
  | Left of pattern
  | Right of pattern

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

type choreo_expr =
  | Unit
  | Var of var_id
  | LocExpr of loc_id * local_expr
  (* | LocSend of loc_id * local_expr * loc_id * var_id * choreo_expr *)
  | Send of choreo_expr * loc_id
  | Sync of loc_id * sync_label * loc_id * choreo_expr
  | If of choreo_expr * choreo_expr * choreo_expr
  | Let of decl_block * choreo_expr
  | FunDef of var_id * choreo_expr
  | FunApp of choreo_expr * choreo_expr
  | Pair of choreo_expr * choreo_expr
  | Fst of choreo_expr
  | Snd of choreo_expr
  | Left of choreo_expr
  | Right of choreo_expr
  | Match of choreo_expr * (pattern * choreo_expr) list

and statement =
  (* | VarDecl of var_id * choreo_type *)
  (* | FunDecl of var_id * choreo_type * choreo_type *)
  (* | LocVarDecl of loc_id * var_id * loc_id * local_type *)
  | VarDecl of pattern * choreo_type
  | TypeDecl of var_id * choreo_type
  | VarAssign of var_id * choreo_expr
  | FunAssign of var_id * pattern list * choreo_expr
  | LocVarAssign of loc_id * var_id * choreo_expr

and decl_block = statement list



type program = Prog of decl_block
