open Local

type choreo_type =
  | TUnit
  | TLoc of loc_id * local_type
  | TSend of choreo_type * choreo_type
  | TProd of choreo_type * choreo_type
  | TSum of choreo_type * choreo_type

type pattern =
  | Default
  | Var of var_id
  | Pair of pattern * pattern
  | LocPatt of loc_id * local_pattern
  | Left of pattern
  | Right of pattern

type choreo_expr =
  | Unit
  | Var of var_id
  | LocExpr of loc_id * local_expr
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
  | Decl of pattern * choreo_type
  | Assign of pattern * choreo_expr
  | TypeDecl of var_id * choreo_type

and decl_block = statement list

type program = Prog of decl_block * filename
