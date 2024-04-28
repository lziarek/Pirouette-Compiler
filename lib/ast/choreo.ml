open Local
open Metainfo

type choreo_type =
  | TUnit of metainfo
  | TLoc of loc_id * local_type * metainfo
  | TSend of choreo_type * choreo_type * metainfo
  | TProd of choreo_type * choreo_type * metainfo
  | TSum of choreo_type * choreo_type * metainfo

type pattern =
  | Default of metainfo
  | Var of var_id * metainfo
  | Pair of pattern * pattern * metainfo
  | LocPatt of loc_id * local_pattern * metainfo
  | Left of pattern * metainfo
  | Right of pattern * metainfo

type choreo_expr =
  | Unit of metainfo
  | Var of var_id * metainfo
  | LocExpr of loc_id * local_expr * metainfo
  | Send of choreo_expr * loc_id * metainfo
  | Sync of loc_id * sync_label * loc_id * choreo_expr * metainfo
  | If of choreo_expr * choreo_expr * choreo_expr * metainfo
  | Let of decl_block * choreo_expr * metainfo
  | FunDef of var_id * choreo_expr * metainfo
  | FunApp of choreo_expr * choreo_expr * metainfo
  | Pair of choreo_expr * choreo_expr * metainfo
  | Fst of choreo_expr * metainfo
  | Snd of choreo_expr * metainfo
  | Left of choreo_expr * metainfo
  | Right of choreo_expr * metainfo
  | Match of choreo_expr * (pattern * choreo_expr) list * metainfo


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
  | TypeDecl of var_id * choreo_type * metainfo

and decl_block = statement list

type program = Prog of decl_block * metainfo

let metainfo_of_ChorTyp = function
  | TUnit m -> m
  | TLoc (_, _, m) -> m
  | TSend (_, _, m) -> m
  | TProd (_, _, m) -> m
  | TSum (_, _, m) -> m

let metainfo_of_Patt = function
  | Default m -> m
  | Var (_, m) -> m
  | Pair (_, _, m) -> m
  | LocPatt (_, _, m) -> m
  | Left (_, m) -> m
  | Right (_, m) -> m

let metainfo_of_ChorExpr = function
  | Unit m -> m
  | Var (_, m) -> m
  | LocExpr (_, _, m) -> m
  | Send (_, _, m) -> m
  | Sync (_, _, _, _, m) -> m
  | If (_, _, _, m) -> m
  | Let (_, _, m) -> m
  | FunDef (_, _, m) -> m
  | FunApp (_, _, m) -> m
  | Pair (_, _, m) -> m
  | Fst (_, m) -> m
  | Snd (_, m) -> m
  | Left (_, m) -> m
  | Right (_, m) -> m
  | Match (_, _, m) -> m

