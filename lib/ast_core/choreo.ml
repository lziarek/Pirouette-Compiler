module Local = Local.M

module M = struct
  type 'a typ_id = Typ_Id of string * 'a

  type 'a typ =
    | TUnit of 'a
    | TLoc of 'a Local.loc_id * 'a Local.typ * 'a
    | TVar of 'a typ_id * 'a
    | TMap of 'a typ * 'a typ * 'a
    | TProd of 'a typ * 'a typ * 'a
    | TSum of 'a typ * 'a typ * 'a

  type 'a pattern =
    | Default of 'a
    | Var of 'a Local.var_id * 'a
    | Pair of 'a pattern * 'a pattern * 'a
    | LocPat of 'a Local.loc_id * 'a Local.pattern * 'a
    | Left of 'a pattern * 'a
    | Right of 'a pattern * 'a

  type 'a expr =
    | Unit of 'a
    | Var of 'a Local.var_id * 'a
    | LocExpr of 'a Local.loc_id * 'a Local.expr * 'a
    | Send of 'a Local.loc_id * 'a expr * 'a Local.loc_id * 'a
    | Sync of 'a Local.loc_id * 'a Local.sync_label * 'a Local.loc_id * 'a expr * 'a
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
    | Assign of 'a pattern list * 'a expr * 'a (* list is only for F P1 P2 ... Pn := C *)
    | TypeDecl of 'a Local.typ_id * 'a typ * 'a

  and 'a stmt_block = 'a stmt list
end

module With (Info : sig
    type t
  end) =
struct
  type nonrec typ_id = Info.t M.typ_id
  type nonrec typ = Info.t M.typ
  type nonrec pattern = Info.t M.pattern
  type nonrec expr = Info.t M.expr
  type nonrec stmt = Info.t M.stmt
  type nonrec stmt_block = stmt list

  let get_info_typid : typ_id -> Info.t = function
    | Typ_Id (_, i) -> i
  ;;

  let get_info_typ : typ -> Info.t = function
    | TUnit i -> i
    | TLoc (_, _, i) -> i
    | TVar (_, i) -> i
    | TMap (_, _, i) -> i
    | TProd (_, _, i) -> i
    | TSum (_, _, i) -> i
  ;;

  let get_info_pattern : pattern -> Info.t = function
    | Default i -> i
    | Var (_, i) -> i
    | Pair (_, _, i) -> i
    | LocPat (_, _, i) -> i
    | Left (_, i) -> i
    | Right (_, i) -> i
  ;;

  let get_info_expr : expr -> Info.t = function
    | Unit i -> i
    | Var (_, i) -> i
    | LocExpr (_, _, i) -> i
    | Send (_, _, _, i) -> i
    | Sync (_, _, _, _, i) -> i
    | If (_, _, _, i) -> i
    | Let (_, _, i) -> i
    | FunDef (_, _, i) -> i
    | FunApp (_, _, i) -> i
    | Pair (_, _, i) -> i
    | Fst (_, i) -> i
    | Snd (_, i) -> i
    | Left (_, i) -> i
    | Right (_, i) -> i
    | Match (_, _, i) -> i
  ;;

  let get_info_stmt : stmt -> Info.t = function
    | Decl (_, _, i) -> i
    | Assign (_, _, i) -> i
    | TypeDecl (_, _, i) -> i
  ;;

  let set_info_typid : Info.t -> typ_id -> typ_id =
    fun i -> function
    | Typ_Id (s, _) -> Typ_Id (s, i)
  ;;

  let set_info_typ : Info.t -> typ -> typ =
    fun i -> function
    | TUnit _ -> TUnit i
    | TLoc (loc, typ, _) -> TLoc (loc, typ, i)
    | TVar (t, _) -> TVar (t, i)
    | TMap (t1, t2, _) -> TMap (t1, t2, i)
    | TProd (t1, t2, _) -> TProd (t1, t2, i)
    | TSum (t1, t2, _) -> TSum (t1, t2, i)
  ;;

  let set_info_pattern : Info.t -> pattern -> pattern =
    fun i -> function
    | Default _ -> Default i
    | Var (x, _) -> Var (x, i)
    | Pair (p1, p2, _) -> Pair (p1, p2, i)
    | LocPat (loc, pat, _) -> LocPat (loc, pat, i)
    | Left (p, _) -> Left (p, i)
    | Right (p, _) -> Right (p, i)
  ;;

  let set_info_expr : Info.t -> expr -> expr =
    fun i -> function
    | Unit _ -> Unit i
    | Var (x, _) -> Var (x, i)
    | LocExpr (loc, e, _) -> LocExpr (loc, e, i)
    | Send (loc1, e, loc2, _) -> Send (loc1, e, loc2, i)
    | Sync (loc1, label, loc2, e, _) -> Sync (loc1, label, loc2, e, i)
    | If (e1, e2, e3, _) -> If (e1, e2, e3, i)
    | Let (stmts, e, _) -> Let (stmts, e, i)
    | FunDef (pats, e, _) -> FunDef (pats, e, i)
    | FunApp (e1, e2, _) -> FunApp (e1, e2, i)
    | Pair (e1, e2, _) -> Pair (e1, e2, i)
    | Fst (e, _) -> Fst (e, i)
    | Snd (e, _) -> Snd (e, i)
    | Left (e, _) -> Left (e, i)
    | Right (e, _) -> Right (e, i)
    | Match (e, cases, _) -> Match (e, cases, i)
  ;;

  let set_info_stmt : Info.t -> stmt -> stmt =
    fun i -> function
    | Decl (pat, typ, _) -> Decl (pat, typ, i)
    | Assign (pats, e, _) -> Assign (pats, e, i)
    | TypeDecl (id, typ, _) -> TypeDecl (id, typ, i)
  ;;
end
