module M : sig
  type 'a value =
    | Int of int * 'a
    | String of string * 'a
    | Bool of bool * 'a

  type 'a loc_id = LocId of string * 'a
  type 'a var_id = VarId of string * 'a
  type 'a typ_id = TypId of string * 'a
  type 'a sync_label = LabelId of string * 'a

  type 'a un_op =
    | Not of 'a
    | Neg of 'a

  type 'a bin_op =
    | Plus of 'a
    | Minus of 'a
    | Times of 'a
    | Div of 'a
    | And of 'a
    | Or of 'a
    | Eq of 'a
    | Neq of 'a
    | Lt of 'a
    | Leq of 'a
    | Gt of 'a
    | Geq of 'a

  type 'a typ =
    | TUnit of 'a
    | TInt of 'a
    | TString of 'a
    | TBool of 'a
    | TVar of 'a typ_id * 'a
    | TProd of 'a typ * 'a typ * 'a
    | TSum of 'a typ * 'a typ * 'a

  type 'a pattern =
    | Default of 'a
    | Val of 'a value * 'a
    | Var of 'a var_id * 'a
    | Pair of 'a pattern * 'a pattern * 'a
    | Left of 'a pattern * 'a
    | Right of 'a pattern * 'a

  type 'a expr =
    | Unit of 'a
    | Val of 'a value * 'a
    | Var of 'a var_id * 'a
    | UnOp of 'a un_op * 'a expr * 'a
    | BinOp of 'a expr * 'a bin_op * 'a expr * 'a
    | Let of 'a var_id * 'a typ * 'a expr * 'a expr * 'a
    | Pair of 'a expr * 'a expr * 'a
    | Fst of 'a expr * 'a
    | Snd of 'a expr * 'a
    | Left of 'a expr * 'a
    | Right of 'a expr * 'a
    | Match of 'a expr * ('a pattern * 'a expr) list * 'a
end

module With : functor
    (Info : sig
       type t
     end)
    -> sig
  type nonrec value = Info.t M.value
  type nonrec loc_id = Info.t M.loc_id
  type nonrec var_id = Info.t M.var_id
  type nonrec typ_id = Info.t M.typ_id
  type nonrec sync_label = Info.t M.sync_label
  type nonrec un_op = Info.t M.un_op
  type nonrec bin_op = Info.t M.bin_op
  type nonrec typ = Info.t M.typ
  type nonrec pattern = Info.t M.pattern
  type nonrec expr = Info.t M.expr

  val get_info_value : value -> Info.t
  val get_info_locid : loc_id -> Info.t
  val get_info_varid : var_id -> Info.t
  val get_info_typid : typ_id -> Info.t
  val get_info_unop : un_op -> Info.t
  val get_info_binop : bin_op -> Info.t
  val get_info_typ : typ -> Info.t
  val get_info_pattern : pattern -> Info.t
  val get_info_expr : expr -> Info.t
  val set_info_value : Info.t -> value -> value
  val set_info_locid : Info.t -> loc_id -> loc_id
  val set_info_varid : Info.t -> var_id -> var_id
  val set_info_typid : Info.t -> typ_id -> typ_id
  val set_info_unop : Info.t -> un_op -> un_op
  val set_info_binop : Info.t -> bin_op -> bin_op
  val set_info_typ : Info.t -> typ -> typ
  val set_info_pattern : Info.t -> pattern -> pattern
  val set_info_expr : Info.t -> expr -> expr
end
