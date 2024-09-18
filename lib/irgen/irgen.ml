let rec epp_stmt (stmt : Ast.Choreo.stmt) (loc : string) : Ast.Net.stmt =
  match stmt with
  | Decl (p, t, _) -> Decl (epp_pattern p loc, epp_type t loc)
  | Assign (ps, e, _) -> Assign (List.map (fun p -> epp_pattern p loc) ps, epp_expr e loc)
  | TypeDecl (id, t, _) -> TypeDecl (id, epp_type t loc)

and epp_expr (c : Ast.Choreo.expr) (loc : string) : Ast.Net.expr =
  match c with
  | LocExpr (LocId (loc1, _), e, _) when loc1 = loc -> Ret e
  | FunDef (ps, c, _) -> FunDef (List.map (fun p -> epp_pattern p loc) ps, epp_expr c loc)
  | FunApp (c1, c2, _) -> FunApp (epp_expr c1 loc, epp_expr c2 loc)
  | Pair (c1, c2, _) -> Pair (epp_expr c1 loc, epp_expr c2 loc)
  | Fst (c, _) -> Fst (epp_expr c loc)
  | Snd (c, _) -> Snd (epp_expr c loc)
  | Left (c, _) -> Left (epp_expr c loc)
  | Right (c, _) -> Right (epp_expr c loc)
  | Let (stmts, c, _) -> Let (List.map (fun stmt -> epp_stmt stmt loc) stmts, epp_expr c loc)
  | Send (LocId (loc1, loc1m), c, LocId (loc2, loc2m), _) ->
    if loc1 = loc2
    then epp_expr c loc
    else if loc1 = loc
    then Send (epp_expr c loc, LocId (loc2, loc2m))
    else if loc2 = loc
    then Recv (LocId (loc1, loc1m))
    else epp_expr c loc
  | Sync (LocId (id1, id1m), l, LocId (id2, id2m), c, _) ->
    if id1 = loc && id2 <> loc
    then ChooseFor (l, LocId (id2, id2m), epp_expr c loc)
    else if id2 = loc && id1 <> loc
    then AllowChoice (LocId (id1, id1m), [ l, epp_expr c loc ])
    else if id1 <> id2
    then epp_expr c loc
    else Unit
  | If (c1, c2, c3, _) ->
    (match merge_expr (epp_expr c2 loc) (epp_expr c3 loc) with
     | Some e -> e
     | None -> If (epp_expr c1 loc, epp_expr c2 loc, epp_expr c3 loc))
  | Match (c, cases, _) ->
    let merged_cases =
      match cases with
      | [] -> None
      | (_, c) :: rest ->
        List.fold_left
          (fun acc (_, c) ->
            match acc with
            | Some e' -> merge_expr e' (epp_expr c loc)
            | None -> None)
          (Some (epp_expr c loc))
          rest
    in
    (match merged_cases with
     | Some e -> e
     | None ->
       Match
         (epp_expr c loc, List.map (fun (p, e) -> epp_pattern p loc, epp_expr e loc) cases))
  | _ -> Unit

and epp_pattern (p : Ast.Choreo.pattern) (loc : string) : Ast.Local.pattern =
  match p with
  | Default m -> Default m
  | Var (id, m) -> Var (id, m)
  | Pair (p1, p2, m) -> Pair (epp_pattern p1 loc, epp_pattern p2 loc, m)
  | LocPatt (LocId (id, _), p, m) -> if id = loc then p else Default m
  | Left (p, m) -> Left (epp_pattern p loc, m)
  | Right (p, m) -> Right (epp_pattern p loc, m)

and epp_type (t : Ast.Choreo.typ) (loc : string) : Ast.Net.typ =
  match t with
  | TLoc (LocId (loc1, _), t1, _) -> if loc1 = loc then TLoc t1 else TUnit
  | TMap (t1, t2, _) -> TMap (epp_type t1 loc, epp_type t2 loc)
  | TProd (t1, t2, _) -> TProd (epp_type t1 loc, epp_type t2 loc)
  | TSum (t1, t2, _) -> TSum (epp_type t1 loc, epp_type t2 loc)
  | _ -> TUnit

(* TODO: change Hashtbl to List *)
and merge_expr (e1 : Ast.Net.expr) (e2 : Ast.Net.expr) : Ast.Net.expr option =
  match e1, e2 with
  | Unit, Unit -> Some Unit
  | Var id1, Var id2 when id1 = id2 -> Some (Var id1)
  | Ret e, Ret e' when e = e' -> Some (Ret e)
  | Pair (e1, e2), Pair (e1', e2') ->
    (match merge_expr e1 e1', merge_expr e2 e2' with
     | Some e1, Some e2 -> Some (Pair (e1, e2))
     | _ -> None)
  | Fst e, Fst e' ->
    (match merge_expr e e' with
     | Some e -> Some (Fst e)
     | None -> None)
  | Snd e, Snd e' ->
    (match merge_expr e e' with
     | Some e -> Some (Snd e)
     | None -> None)
  | Left e, Left e' ->
    (match merge_expr e e' with
     | Some e -> Some (Left e)
     | None -> None)
  | Right e, Right e' ->
    (match merge_expr e e' with
     | Some e -> Some (Right e)
     | None -> None)
  | Send (e1, LocId (loc, locm)), Send (e2, LocId (loc', _)) when loc = loc' ->
    (match merge_expr e1 e2 with
     | Some e -> Some (Send (e, LocId (loc, locm)))
     | None -> None)
  | Recv (LocId (loc, locm)), Recv (LocId (loc', _)) when loc = loc' -> Some (Recv (LocId (loc, locm)))
  | FunDef (p, e), FunDef (p', e') when p = p' && e = e' -> Some (FunDef (p, e))
  | FunApp (e1, e2), FunApp (e1', e2') ->
    (match merge_expr e1 e1', merge_expr e2 e2' with
     | Some e1, Some e2 -> Some (FunApp (e1, e2))
     | _ -> None)
  | If (e1, e2, e3), If (e1', e2', e3') when e1 = e1' ->
    (match merge_expr e2 e2', merge_expr e3 e3' with
     | Some e2, Some e3 -> Some (If (e1, e2, e3))
     | _ -> None)
  | Let (stmts1, e1), Let (stmts2, e2) ->
    if List.length stmts1 <> List.length stmts2
    then None
    else (
      let merged_stmts =
        let exception Not_matched in
        try
          List.fold_left2
            (fun acc s1 s2 ->
              match acc with
              | Some acc ->
                (match merge_stmt s1 s2 with
                 | Some s -> Some (s :: acc)
                 | None -> raise Not_matched)
              | None -> raise Not_matched)
            (Some [])
            stmts1
            stmts2
        with
        | Not_matched -> None
      in
      match merged_stmts with
      | Some stmts ->
        (match merge_expr e1 e2 with
         | Some e -> Some (Let (List.rev stmts, e))
         | None -> None)
      | None -> None)
  | Match (e, cases1), Match (e', cases2) when e = e' ->
    let cases1_tbl = Hashtbl.create (List.length cases1) in
    List.iter (fun (p, e) -> Hashtbl.add cases1_tbl p e) cases1;
    let exception Not_matched in
    (try
       List.iter
         (fun (p, e) ->
           match Hashtbl.find_opt cases1_tbl p with
           | Some e' ->
             (match merge_expr e e' with
              | Some e -> Hashtbl.replace cases1_tbl p e
              | None -> raise Not_matched)
           | None -> raise Not_matched)
         cases2;
       Some (Match (e, Hashtbl.fold (fun p e acc -> (p, e) :: acc) cases1_tbl []))
     with
     | Not_matched -> None)
  | ChooseFor (l, LocId (loc, locm), e1), ChooseFor (l', LocId (loc', _), e2) when l = l' && loc = loc'
    ->
    (match merge_expr e1 e2 with
     | Some e -> Some (ChooseFor (l, LocId (loc, locm), e))
     | None -> None)
  | AllowChoice (LocId (loc, locm), choices1), AllowChoice (LocId (loc', _), choices2) when loc = loc'
    ->
    Some
      (AllowChoice
         ( LocId (loc, locm)
         , let h = Hashtbl.create 2 in
           List.iter (merge_choice_into h) choices1;
           List.iter (merge_choice_into h) choices2;
           Hashtbl.fold (fun l e acc -> (l, e) :: acc) h [] ))
    (* use list *)
  | _ -> None

and merge_stmt (s1 : Ast.Net.stmt) (s2 : Ast.Net.stmt) : Ast.Net.stmt option =
  match s1, s2 with
  | Decl (p, t), Decl (p', t') when p = p' && t = t' -> Some (Decl (p, t))
  | TypeDecl (id, t), TypeDecl (id', t') when id = id' && t = t' ->
    Some (TypeDecl (id, t))
  | Assign (p, e1), Assign (p', e2) when p = p' ->
    (match merge_expr e1 e2 with
     | Some e -> Some (Assign (p, e))
     | None -> None)
  | _ -> None

and merge_choice_into tbl (label, expr) =
  match Hashtbl.find_opt tbl label with
  | Some expr' ->
    (match merge_expr expr expr' with
     | Some e -> Hashtbl.replace tbl label e
     | None -> Hashtbl.add tbl label expr)
  | None -> Hashtbl.add tbl label expr
;;

let epp (Ast.Choreo.Prog (prog, _)) loc =
  Ast.Net.Prog (List.map (fun stmt -> epp_stmt stmt loc) prog)
;;
