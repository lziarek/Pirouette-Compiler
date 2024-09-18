type typ =
  | TUnit of Metainfo.metainfo
  | TLoc of Local.loc_id * Local.typ * Metainfo.metainfo
  | TMap of typ * typ * Metainfo.metainfo
  | TProd of typ * typ * Metainfo.metainfo
  | TSum of typ * typ * Metainfo.metainfo
  | TAlias of Local.typ_id * typ * Metainfo.metainfo

type pattern =
  | Default of Metainfo.metainfo
  | Var of Local.var_id * Metainfo.metainfo
  | Pair of pattern * pattern * Metainfo.metainfo
  | LocPatt of Local.loc_id * Local.pattern * Metainfo.metainfo
  | Left of pattern * Metainfo.metainfo
  | Right of pattern * Metainfo.metainfo

type expr =
  | Unit of Metainfo.metainfo
  | Var of Local.var_id * Metainfo.metainfo
  | LocExpr of Local.loc_id * Local.expr * Metainfo.metainfo
  | Send of Local.loc_id * expr * Local.loc_id * Metainfo.metainfo
  | Sync of Local.loc_id * Local.sync_label * Local.loc_id * expr * Metainfo.metainfo
  | If of expr * expr * expr * Metainfo.metainfo
  | Let of stmt_block * expr * Metainfo.metainfo
  | FunDef of pattern list * expr * Metainfo.metainfo
  | FunApp of expr * expr * Metainfo.metainfo
  | Pair of expr * expr * Metainfo.metainfo
  | Fst of expr * Metainfo.metainfo
  | Snd of expr * Metainfo.metainfo
  | Left of expr * Metainfo.metainfo
  | Right of expr * Metainfo.metainfo
  | Match of expr * (pattern * expr) list * Metainfo.metainfo

(** The [statement] type represents different kinds of statements in a choreography language's AST.

    Each variant of the type corresponds to a specific kind of statement:
    
    - [Decl (pattern, choreo_type, metainfo)]: Declares a variable of a specified type.
      Example: [Decl (Var "x", TUnit, meta_info)] declares a unit type variable "x".
    
    - [Assign (pattern, choreo_expr, metainfo)]: Assigns the result of a choreography expression to a pattern.
      Example: [Assign (Var "x", Unit, meta_info)] assigns the unit value to variable "x".
    
    The [metainfo] is defined in [local.ml].
        
    This type is crucial for representing the operations and transformations within a choreography language's program structure.
**)
and stmt =
  | Decl of pattern * typ * Metainfo.metainfo
  | Assign of pattern list * expr * Metainfo.metainfo
  | TypeDecl of Local.typ_id * typ * Metainfo.metainfo

and stmt_block = stmt list

type program = Prog of stmt_block * Metainfo.metainfo

let metainfo_of_ChorTyp = function
  | TUnit m -> m
  | TLoc (_, _, m) -> m
  | TMap (_, _, m) -> m
  | TProd (_, _, m) -> m
  | TSum (_, _, m) -> m
  | TAlias (_, _, m) -> m

let metainfo_of_ChorPatt = function
  | Default m -> m
  | Var (_, m) -> m
  | Pair (_, _, m) -> m
  | LocPatt (_, _, m) -> m
  | Left (_, m) -> m
  | Right (_, m) -> m

let metainfo_of_ChorPatt_list = function
  | patt :: _ -> metainfo_of_ChorPatt patt
  | _ ->  failwith "Empty pattern list"

let metainfo_of_ChorExpr = function
  | Unit m -> m
  | Var (_, m) -> m
  | LocExpr (_, _, m) -> m
  | Send (_, _, _, m) -> m
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