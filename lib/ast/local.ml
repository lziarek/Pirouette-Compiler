type value =
  [ `Int of int * Metainfo.metainfo
  | `String of string * Metainfo.metainfo
  | `Bool of bool * Metainfo.metainfo
  ]

type loc_id = LocId of string * Metainfo.metainfo
type var_id = VarId of string * Metainfo.metainfo
type typ_id = TypId of string * Metainfo.metainfo
type sync_label = LabelId of string * Metainfo.metainfo

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

  Each variant is tagged with a `Metainfo.metainfo` (see above).
*)
type un_op =
  | Not of Metainfo.metainfo
  | Neg of Metainfo.metainfo

type bin_op =
  | Plus of Metainfo.metainfo
  | Minus of Metainfo.metainfo
  | Times of Metainfo.metainfo
  | Div of Metainfo.metainfo
  | And of Metainfo.metainfo
  | Or of Metainfo.metainfo
  | Eq of Metainfo.metainfo
  | Neq of Metainfo.metainfo
  | Lt of Metainfo.metainfo
  | Leq of Metainfo.metainfo
  | Gt of Metainfo.metainfo
  | Geq of Metainfo.metainfo

type typ =
  | TUnit of Metainfo.metainfo
  | TInt of Metainfo.metainfo
  | TString of Metainfo.metainfo
  | TBool of Metainfo.metainfo
  | TProd of typ * typ * Metainfo.metainfo
  | TSum of typ * typ * Metainfo.metainfo

type pattern =
  | Default of Metainfo.metainfo
  | Val of value * Metainfo.metainfo
  | Var of var_id * Metainfo.metainfo
  | Pair of pattern * pattern * Metainfo.metainfo
  | Left of pattern * Metainfo.metainfo
  | Right of pattern * Metainfo.metainfo

type expr =
  | Unit of Metainfo.metainfo
  | Val of value * Metainfo.metainfo
  | Var of var_id * Metainfo.metainfo
  | UnOp of un_op * expr * Metainfo.metainfo
  | BinOp of expr * bin_op * expr * Metainfo.metainfo
  | Let of var_id * expr * expr * Metainfo.metainfo
  | Pair of expr * expr * Metainfo.metainfo
  | Fst of expr * Metainfo.metainfo
  | Snd of expr * Metainfo.metainfo
  | Left of expr * Metainfo.metainfo
  | Right of expr * Metainfo.metainfo
  | Match of expr * (pattern * expr) list * Metainfo.metainfo

let metainfo_of_Val = function
  | `Int (_, m) -> m
  | `String (_, m) -> m
  | `Bool (_, m) -> m

let metainfo_of_LocId (LocId (_, m)) = m
let metainfo_of_VarId (VarId (_, m)) = m
let metainfo_of_TypId (TypId (_, m)) = m
let metainfo_of_LabelId (LabelId (_, m)) = m

let metainfo_of_UnOp = function
  | Not m -> m
  | Neg m -> m
  
let metainfo_of_LocTyp = function
  | TUnit m -> m
  | TInt m -> m
  | TString m -> m
  | TBool m -> m
  | TProd (_, _, m) -> m
  | TSum (_, _, m) -> m

let metainfo_of_LocPatt = function
  | Default m -> m
  | Val (_, m) -> m
  | Var (_, m) -> m
  | Pair (_, _, m) -> m
  | Left (_, m) -> m
  | Right (_, m) -> m

let metainfo_of_LocExpr = function
  | Unit m -> m
  | Val (_, m) -> m
  | Var (_, m) -> m
  | UnOp (_, _, m) -> m
  | BinOp (_, _, _, m) -> m
  | Let (_, _, _, m) -> m
  | Pair (_, _, m) -> m
  | Fst (_, m) -> m
  | Snd (_, m) -> m
  | Left (_, m) -> m
  | Right (_, m) -> m
  | Match (_, _, m) -> m