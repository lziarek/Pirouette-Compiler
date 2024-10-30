module M = struct
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

module With (Info : sig
    type t
  end) =
struct
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

  let get_info_value : value -> Info.t = function
    | Int (_, i) -> i
    | String (_, i) -> i
    | Bool (_, i) -> i
  ;;

  let get_info_locid : loc_id -> Info.t = function
    | LocId (_, i) -> i
  ;;

  let get_info_varid : var_id -> Info.t = function
    | VarId (_, i) -> i
  ;;

  let get_info_typid : typ_id -> Info.t = function
    | TypId (_, i) -> i
  ;;

  let get_info_unop : un_op -> Info.t = function
    | Not i -> i
    | Neg i -> i
  ;;

  let get_info_binop : bin_op -> Info.t = function
    | Plus i -> i
    | Minus i -> i
    | Times i -> i
    | Div i -> i
    | And i -> i
    | Or i -> i
    | Eq i -> i
    | Neq i -> i
    | Lt i -> i
    | Leq i -> i
    | Gt i -> i
    | Geq i -> i
  ;;

  let get_info_typ : typ -> Info.t = function
    | TUnit i -> i
    | TInt i -> i
    | TString i -> i
    | TBool i -> i
    | TVar (_, i) -> i
    | TProd (_, _, i) -> i
    | TSum (_, _, i) -> i
  ;;

  let get_info_pattern : pattern -> Info.t = function
    | Default i -> i
    | Val (_, i) -> i
    | Var (_, i) -> i
    | Pair (_, _, i) -> i
    | Left (_, i) -> i
    | Right (_, i) -> i
  ;;

  let get_info_expr : expr -> Info.t = function
    | Unit i -> i
    | Val (_, i) -> i
    | Var (_, i) -> i
    | UnOp (_, _, i) -> i
    | BinOp (_, _, _, i) -> i
    | Let (_, _, _, _, i) -> i
    | Pair (_, _, i) -> i
    | Fst (_, i) -> i
    | Snd (_, i) -> i
    | Left (_, i) -> i
    | Right (_, i) -> i
    | Match (_, _, i) -> i
  ;;

  let set_info_value : Info.t -> value -> value =
    fun i -> function
    | Int (n, _) -> Int (n, i)
    | String (s, _) -> String (s, i)
    | Bool (b, _) -> Bool (b, i)
  ;;

  let set_info_locid : Info.t -> loc_id -> loc_id =
    fun i -> function
    | LocId (s, _) -> LocId (s, i)
  ;;

  let set_info_varid : Info.t -> var_id -> var_id =
    fun i -> function
    | VarId (s, _) -> VarId (s, i)
  ;;

  let set_info_typid : Info.t -> typ_id -> typ_id =
    fun i -> function
    | TypId (s, _) -> TypId (s, i)
  ;;

  let set_info_unop : Info.t -> un_op -> un_op =
    fun i -> function
    | Not _ -> Not i
    | Neg _ -> Neg i
  ;;

  let set_info_binop : Info.t -> bin_op -> bin_op =
    fun i -> function
    | Plus _ -> Plus i
    | Minus _ -> Minus i
    | Times _ -> Times i
    | Div _ -> Div i
    | And _ -> And i
    | Or _ -> Or i
    | Eq _ -> Eq i
    | Neq _ -> Neq i
    | Lt _ -> Lt i
    | Leq _ -> Leq i
    | Gt _ -> Gt i
    | Geq _ -> Geq i
  ;;

  let set_info_typ : Info.t -> typ -> typ =
    fun i -> function
    | TUnit _ -> TUnit i
    | TInt _ -> TInt i
    | TString _ -> TString i
    | TBool _ -> TBool i
    | TVar (t, _) -> TVar (t, i)
    | TProd (t1, t2, _) -> TProd (t1, t2, i)
    | TSum (t1, t2, _) -> TSum (t1, t2, i)
  ;;

  let set_info_pattern : Info.t -> pattern -> pattern =
    fun i -> function
    | Default _ -> Default i
    | Val (v, _) -> Val (v, i)
    | Var (x, _) -> Var (x, i)
    | Pair (p1, p2, _) -> Pair (p1, p2, i)
    | Left (p, _) -> Left (p, i)
    | Right (p, _) -> Right (p, i)
  ;;

  let set_info_expr : Info.t -> expr -> expr =
    fun i -> function
    | Unit _ -> Unit i
    | Val (v, _) -> Val (v, i)
    | Var (x, _) -> Var (x, i)
    | UnOp (op, e, _) -> UnOp (op, e, i)
    | BinOp (e1, op, e2, _) -> BinOp (e1, op, e2, i)
    | Let (x, t, e1, e2, _) -> Let (x, t, e1, e2, i)
    | Pair (e1, e2, _) -> Pair (e1, e2, i)
    | Fst (e, _) -> Fst (e, i)
    | Snd (e, _) -> Snd (e, i)
    | Left (e, _) -> Left (e, i)
    | Right (e, _) -> Right (e, i)
    | Match (e, cases, _) -> Match (e, cases, i)
  ;;
end
