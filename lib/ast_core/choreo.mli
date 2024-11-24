module M : sig
  type 'a typ_id = Typ_Id of string * 'a

  type 'a typ =
    | TUnit of 'a
    | TLoc of 'a Local.M.loc_id * 'a Local.M.typ * 'a
    | TVar of 'a typ_id * 'a
    | TMap of 'a typ * 'a typ * 'a
    | TProd of 'a typ * 'a typ * 'a
    | TSum of 'a typ * 'a typ * 'a

  type 'a pattern =
    | Default of 'a
    | Var of 'a Local.M.var_id * 'a
    | Pair of 'a pattern * 'a pattern * 'a
    | LocPat of 'a Local.M.loc_id * 'a Local.M.pattern * 'a
    | Left of 'a pattern * 'a
    | Right of 'a pattern * 'a

  type 'a expr =
    | Unit of 'a
    | Var of 'a Local.M.var_id * 'a
    | LocExpr of 'a Local.M.loc_id * 'a Local.M.expr * 'a
    | Send of 'a Local.M.loc_id * 'a expr * 'a Local.M.loc_id * 'a
    | Sync of 'a Local.M.loc_id * 'a Local.M.sync_label * 'a Local.M.loc_id * 'a expr * 'a
    | If of 'a expr * 'a expr * 'a expr * 'a
    | Let of 'a stmt_block * 'a expr * 'a
    | FunDef of 'a pattern list * 'a expr * 'a
    | FunApp of 'a expr * 'a expr * 'a
    | Pair of 'a expr * 'a expr * 'a
    | Fst of 'a expr * 'a
    | Snd of 'a expr * 'a
    | Left of 'a expr * 'a
    | Right of 'a expr * 'a
    | Match of 'a expr * ('a pattern * 'a expr) list * 'a

  and 'a stmt =
    | Decl of 'a pattern * 'a typ * 'a
    | Assign of 'a pattern list * 'a expr * 'a
    | TypeDecl of 'a Local.M.typ_id * 'a typ * 'a

  and 'a stmt_block = 'a stmt list
end

module With : functor
    (Info : sig
       type t
     end)
    -> sig
  type nonrec typ_id = Info.t M.typ_id
  type nonrec typ = Info.t M.typ
  type nonrec pattern = Info.t M.pattern
  type nonrec expr = Info.t M.expr
  type nonrec stmt = Info.t M.stmt
  type nonrec stmt_block = stmt list

  val get_info_typid : typ_id -> Info.t
  val get_info_typ : typ -> Info.t
  val get_info_pattern : pattern -> Info.t
  val get_info_expr : expr -> Info.t
  val get_info_stmt : stmt -> Info.t
  val set_info_typid : Info.t -> typ_id -> typ_id
  val set_info_typ : Info.t -> typ -> typ
  val set_info_pattern : Info.t -> pattern -> pattern
  val set_info_expr : Info.t -> expr -> expr
  val set_info_stmt : Info.t -> stmt -> stmt
end
