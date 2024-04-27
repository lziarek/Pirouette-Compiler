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


  (** The [statement] type represents different kinds of statements in a choreography language's AST.

    Each variant of the type corresponds to a specific kind of statement:
    
    - [Decl (pattern, choreo_type, metainfo)]: Declares a variable of a specified type.
      Example: [Decl (Var "x", TUnit, meta_info)] declares a unit type variable "x".
    
    - [Assign (pattern, choreo_expr, metainfo)]: Assigns the result of a choreography expression to a pattern.
      Example: [Assign (Var "x", Unit, meta_info)] assigns the unit value to variable "x".
    
    The [metainfo] is defined in [local.ml].
        
    This type is crucial for representing the operations and transformations within a choreography language's program structure.
**)

and statement =
  | Decl of pattern * choreo_type * metainfo
  | Assign of pattern * choreo_expr * metainfo
  | TypeDecl of var_id * choreo_type

and decl_block = statement list

type program = Prog of decl_block * metainfo
