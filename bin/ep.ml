open Ctrl
open Expr


module SS = Set.Make(String);;

(* let check_str s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false *)

(* let parse_eq parsed_lft parsed_rght op : string = 
  let isStrL = check_str parsed_lft in
  let isStrR = check_str parsed_rght in
  if isStrL = true &&  isStrR = true then
  "(" ^ parsed_lft ^ " " ^ op ^ " " ^ parsed_rght ^ ")"
  else parsed_lft ^ " " ^ op ^ " " ^ parsed_rght *)
(* 
let rec parse_expr expr currentNode : string =
  match expr with
  | Branch { ift = Assoc { loc; arg }; thn; el } ->
      let parsed_arg = parse_expr arg currentNode in
      let parsed_thn = parse_expr thn currentNode in
      let parsed_el = parse_expr el currentNode in
      if currentNode = loc then 
        "if " ^ parsed_arg ^ " then " ^ parsed_thn ^ " else " ^ parsed_el
    else
      " ********** MERGE REQD ********** "
  | Branch { ift = _; thn = _; el = _} -> ""
  | Sync { sndr; d; rcvr; thn} ->
    if currentNode = sndr && currentNode = rcvr then
      ""
    else if currentNode = sndr && currentNode != rcvr then
      let parsed_el = parse_expr thn currentNode in
        "choose " ^ d ^ " for " ^ rcvr ^ "; \n" ^ parsed_el
    else if currentNode = rcvr && currentNode != sndr then
      let parsed_el = parse_expr thn currentNode in
        "allow " ^ sndr ^ " choice | " ^ d ^ " => " ^ parsed_el
    else
      parse_expr thn currentNode
  | Assoc { loc; arg } ->
      let parsed_arg = parse_expr arg currentNode in
      if currentNode = loc then
        "ret(" ^ parsed_arg ^ ")"
      else 
        "()"
  | Variable x -> x
  | Plus {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "+"
  | Minus {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "-"
  | Product {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "*"
  | Division {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "/"
  | Condition {lft; op; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght op
  | Value x -> string_of_int x
  | Map {name; arg} -> 
    let parsed_arg = parse_expr arg currentNode in
    name ^ "[" ^ parsed_arg ^ "]"
  | Let {fst = Assoc{loc = _; arg = arg_fst}; snd = Snd {sndr = Assoc {loc; arg = arg_snd}; name}; thn} ->
    let parsed_thn = parse_expr thn currentNode in
    let parsed_arg_fst = parse_expr arg_fst currentNode in
    let parsed_arg_snd = parse_expr arg_snd currentNode in
    if name = currentNode && name = loc then ""
    else if name != currentNode && currentNode = loc then
      "send " ^ parsed_arg_snd ^ " to " ^ name ^ "; \n" ^ parsed_thn
    else if name = currentNode && currentNode != loc then
      "receive " ^ parsed_arg_fst ^ " from " ^ loc ^ "; \n" ^ parsed_thn
    else
      parsed_thn
  | Let {fst = Assoc{loc; arg}; snd; thn} ->
    let parsed_arg = parse_expr arg currentNode in
    let parsed_snd = parse_expr snd currentNode in 
    let parsed_thn = parse_expr thn currentNode in
    if loc = currentNode then 
      "let ret(" ^ parsed_arg ^ ") := " ^ parsed_snd ^ " in " ^ parsed_thn
    else "( fun_g F(X) := " ^ parsed_thn ^ ") " ^ parsed_snd
  | Let {fst = _; snd = _; thn = _} -> ""
  | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
    let parsed_body = parse_expr body currentNode in
    let parsed_arg = parse_expr arg2 currentNode in
    if loc = currentNode then
      "fun_l " ^ name ^ "(" ^ parsed_arg ^ ") := " ^ parsed_body
    else
      "fun_g " ^ name ^ "(X) := " ^ parsed_body
  | Fun {name; arg = ChoreoVars x; body} -> 
    let parsed_body = parse_expr body currentNode in
    "fun_g " ^ name ^ "("^ x ^") := " ^ parsed_body
  | Fun {name = _; arg = _; body = _} -> ""
  | Application {funct; argument = Assoc {loc; arg}} -> 
    let parsed_funct = parse_expr funct currentNode in
    let parsed_arg = parse_expr arg currentNode in
    if loc = currentNode then
      parsed_funct ^ " " ^ parsed_arg
    else
      parsed_funct ^ " ()"
  | Application {funct; argument} -> 
    let parsed_funct = parse_expr funct currentNode in
    let parsed_argument = parse_expr argument currentNode in
      parsed_funct ^ " " ^ parsed_argument
  | ChoreoVars x -> x 
  | Snd _ | Abstraction _ | Comm_S _ | UMinus _ -> ""  *)

let get_entitities expr : SS.t = 
  let set1 = SS.empty in
    let rec aux acc expr = match expr with
      | Branch { ift; thn; el } -> 
        let acc_ift = aux acc ift in
        let acc_thn = aux acc_ift thn in
        let acc_el = aux acc_thn el in
          acc_el
      | Sync {sndr; d = _; rcvr; thn} ->
        let acc = aux acc thn in
          let acc = SS.union (SS.add sndr acc) (SS.add rcvr acc) in 
            acc
      | Assoc {loc; arg = _} ->
          (SS.add loc acc)
      | Fun {name = _; arg; body} ->
        let acc_arg = aux acc arg in
        let acc = aux acc_arg body in
          acc
      | Snd {sndr; name} -> 
        let acc_sndr = aux acc sndr in
          (SS.add name acc_sndr)
      | Let {fst; snd; thn}  ->
        let acc_fst = aux acc fst in
        let acc_snd = aux acc_fst snd in
        let acc_thn = aux acc_snd thn in
          acc_thn
      | Application {funct; argument} -> 
        let acc_funct = aux acc funct in
        let acc = aux acc_funct argument in
          acc
      | Variable _ | Value _ | ChoreoVars _ | Condition _ | Map _ | Abstraction _
      | Comm_S _ | Plus _|Minus _|Product _|Division _| UMinus _-> acc
    in
      aux set1 expr

let ast = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
snd =
Expr.Snd {
  sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")};
  name = "Person2"};
thn =
Expr.Application {
  funct =
  Expr.Fun {name = "initpay"; arg = (Expr.ChoreoVars "X");
    body =
    Expr.Branch {
      ift =
      Expr.Assoc {loc = "Person2";
        arg =
        Expr.Condition {lft = (Expr.Variable "d"); op = "<";
          rght = (Expr.Value 500)}};
      thn =
      Expr.Branch {
        ift =
        Expr.Assoc {loc = "Person2";
          arg =
          Expr.Condition {lft = (Expr.Variable "d"); op = "<";
            rght = (Expr.Value 300)}};
        thn =
        Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1";
          thn = (Expr.ChoreoVars "X")};
        el =
        Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1";
          thn = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)}}};
      el =
      Expr.Branch {
        ift =
        Expr.Assoc {loc = "Person2";
          arg =
          Expr.Condition {lft = (Expr.Variable "d"); op = "<";
            rght = (Expr.Value 500)}};
        thn =
        Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1";
          thn = (Expr.ChoreoVars "Y")};
        el =
        Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1";
          thn = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 1)}}}}};
  argument = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "d")}}}
(* let ast = (Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")}; name = "Person2"};
thn = Expr.Application { funct = Expr.Fun {name = "initpay"; arg = (Expr.ChoreoVars "X");
body = Expr.Branch { ift = Expr.Assoc {loc = "Person2"; arg = Expr.Condition {lft = (Expr.Variable "x"); op = "<"; rght = (Expr.Value 500)}};
thn = Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1"; thn = Expr.Let { fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "rem")}; name = "Person2"};
thn = Expr.Let { fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")};
snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = Expr.Minus {lft = (Expr.Variable "amt_due");  rght = (Expr.Variable "rem")}}; 
name = "Person2"}; thn = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}};
el = Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1"; thn = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 500)}; name = "Person2"};
thn = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")}; 
snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)}; name = "Person2"};thn = (Expr.ChoreoVars "X")}}}}};
argument = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}) *)

let entities : SS.t = 
  get_entitities ast
  
(* let () = SS.iter (fun entity -> 
  let res = parse_expr ast entity in
  let str = "____________________________" ^ entity ^ "___________________________________" in 
  print_endline str;
  print_endline res;
  ) entities *)


let rec merge_branch (lbranch:ctrl) (rbranch:ctrl) : ctrl= 
  match lbranch, rbranch with
    | ChoreoVars x, ChoreoVars y when x = y -> ChoreoVars x
    | Unit, Unit -> Unit
    | Ret x, Ret y when x = y -> Ret x
    | Branch {ift; thn; el}, Branch {ift = ift2; thn = thn2; el = el2} 
      when ift = ift2 ->
        let merged_thn = merge_branch thn thn2 in
        let merged_el = merge_branch el el2 in
        Branch {ift; thn = merged_thn; el = merged_el}
    | Snd {arg; loc; thn}, Snd {arg = arg2; loc = loc2; thn = thn2} 
      when arg = arg2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        Snd {arg; loc; thn = merged_thn}
    | Rcv {arg; loc; thn}, Rcv {arg = arg2; loc = loc2; thn = thn2} 
      when arg = arg2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        Rcv {arg; loc; thn = merged_thn}
    | Choose {d; loc; thn}, Choose {d = d2; loc = loc2; thn = thn2} 
      when d = d2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        Choose {d; loc; thn = merged_thn}
    (* LL *)
    | Allow {from; l = SyncLabel{d; thn}; r = None}, Allow {from = from2; l = SyncLabel {d = d2; thn = thn2}; r = None} 
      when d = d2 && from = from2 ->
        let merged_thn = merge_branch thn thn2 in
        Allow {from; l = SyncLabel{d; thn = merged_thn}; r = None}
    (* LR *)
    | Allow {from; l = SyncLabel{d; thn}; r = None}, Allow {from = from2; l = None; r = SyncLabel {d = d2; thn = thn2}} 
      when from = from2 ->
        Allow {from; l = SyncLabel{d; thn}; r = SyncLabel{d = d2; thn = thn2}}
    (* LLR *)
    | Allow {from; l = SyncLabel{d; thn}; r = None}, Allow {from = from2; l = SyncLabel {d = d2; thn = thn2}; r = SyncLabel {d = d3; thn = thn3}} 
      when d = d2 && from = from2 && d2 != d3 ->
        let merged_thn = merge_branch thn thn2 in
        Allow {from; l = SyncLabel{d; thn = merged_thn}; r = SyncLabel{d = d3; thn = thn3}}
    (* RL *)
    | Allow {from; l = None; r = SyncLabel{d; thn}}, Allow {from = from2; l = SyncLabel {d = d2; thn = thn2}; r = None} 
      when d != d2 && from = from2 ->
        Allow {from; l = SyncLabel{d = d2; thn = thn2}; r = SyncLabel{d; thn}}
    (* RR *)
    | Allow {from; l = None; r = SyncLabel{d; thn}}, Allow {from = from2; l = None; r = SyncLabel {d = d2; thn = thn2}} 
      when from = from2 && d = d2->
        let merged_thn = merge_branch thn thn2 in
        Allow {from; l = None; r = SyncLabel{d; thn = merged_thn}}
    (* RLR *)
    | Allow {from; l = None; r = SyncLabel{d; thn}}, Allow {from = from2; l = SyncLabel {d = d2; thn = thn2}; r = SyncLabel {d = d3; thn = thn3}} 
      when d = d3 && from = from2 && d2 != d3 ->
        let merged_thn = merge_branch thn thn3 in
        Allow {from; l = SyncLabel{d = d2; thn = thn2}; r = SyncLabel{d; thn = merged_thn}}
    (* LRL *)
    | Allow {from; l = SyncLabel{d; thn}; r = SyncLabel{d = d2; thn = thn2}}, Allow {from = from2; l = SyncLabel {d = d3; thn = thn3}; r = None} 
      when d = d3 && from = from2 && d2 != d3 ->
        let merged_thn = merge_branch thn thn3 in
        Allow {from; l = SyncLabel{d; thn = merged_thn}; r = SyncLabel{d; thn = thn2}}
    (* LRR *)
    | Allow {from; l = SyncLabel{d; thn}; r = SyncLabel{d = d2; thn = thn2}}, Allow {from = from2; l = None; r = SyncLabel {d = d3; thn = thn3}} 
      when d2 = d3 && from = from2 && d != d3 ->
        let merged_thn = merge_branch thn2 thn3 in
        Allow {from; l = SyncLabel{d; thn}; r = SyncLabel{d = d2; thn = merged_thn}}
    (* LRLR *)
    | Allow {from; l = SyncLabel{d; thn}; r = SyncLabel{d = d2; thn = thn2}}, Allow {from = from2; l = SyncLabel {d = d3; thn = thn3}; r = SyncLabel {d = d4; thn = thn4}} 
      when d = d3 && d2 = d4 && from = from2 ->
        let merged_thn_l = merge_branch thn thn3 in
        let merged_thn_r = merge_branch thn2 thn4 in
        Allow {from; l = SyncLabel{d; thn = merged_thn_l}; r = SyncLabel{d = d2; thn = merged_thn_r}}
    | Let {binder; arg; thn}, Let {binder = binder2; arg = arg2; thn = thn2}
        when binder = binder2 -> 
        let merged_arg = merge_branch arg arg2 in
        let merged_thn = merge_branch thn thn2 in
        Let {binder; arg = merged_arg; thn = merged_thn}
    | Fun {name; arg; body}, Fun {name = name2; arg = arg2; body = body2} 
        when name = name2 && arg = arg2 && body = body2 ->
          Fun {name; arg; body}
    | Application {funct; argument}, Application {funct = funct2; argument = argument2} ->
      let merged_funct = merge_branch funct funct2 in
      let merged_argument = merge_branch argument argument2 in
          Application {funct = merged_funct; argument = merged_argument}
    | _ -> None

  (* let rec parse_ast expr currentNode = *)
let rec parse_ast expr_ast currentNode =
  match expr_ast with
  | Assoc { loc; arg } ->
    let parsed_arg = parse_ast arg currentNode in
    if currentNode = loc then
      Ret parsed_arg
    else 
      Unit
  | Branch { ift = Assoc { loc; arg }; thn; el } ->
      let parsed_arg = parse_ast arg currentNode in
      let parsed_thn = parse_ast thn currentNode in
      let parsed_el = parse_ast el currentNode in
      if currentNode = loc then 
        Branch {ift = parsed_arg; thn = parsed_thn; el = parsed_el}
      else
        merge_branch parsed_thn parsed_el
  | Branch { ift = _; thn = _; el = _} -> None
  | Sync { sndr; d; rcvr; thn} ->
    if currentNode = sndr && currentNode = rcvr then
      None
    else if currentNode = sndr && currentNode != rcvr then
      let parsed_el = parse_ast thn currentNode in
        Choose {d; loc = rcvr; thn = parsed_el}
    else if currentNode = rcvr && currentNode != sndr then
      let parsed_el = parse_ast thn currentNode in
        if d = "L" then 
          Allow {from= sndr; l = SyncLabel {d; thn = parsed_el}; r = None}
        else
          Allow {from= sndr; l = None; r = SyncLabel {d; thn = parsed_el}}
    else
      parse_ast thn currentNode
  | Variable x -> Variable x
  | Plus {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    Plus {lft =  parsed_lft; rght = parsed_rght}
  | Minus {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    Minus {lft =  parsed_lft; rght = parsed_rght}
  | Product {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    Product {lft =  parsed_lft; rght = parsed_rght}
  | Division {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    Division {lft =  parsed_lft; rght = parsed_rght}
  | Condition {lft; op; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    Condition {lft = parsed_lft; op; rght = parsed_rght}
  | Value x -> Value x
  | Map {name; arg} -> 
    let parsed_arg = parse_ast arg currentNode in
    Map {name; arg = parsed_arg}
  | Let {fst = Assoc{loc = _; arg = arg_fst}; snd = Snd {sndr = Assoc {loc; arg = arg_snd}; name}; thn} ->
    let parsed_thn = parse_ast thn currentNode in
    let parsed_arg_fst = parse_ast arg_fst currentNode in
    let parsed_arg_snd = parse_ast arg_snd currentNode in
    if name = currentNode && name = loc then None
    else if name != currentNode && currentNode = loc then
      Snd {arg = parsed_arg_snd; loc = name; thn = parsed_thn}
    else if name = currentNode && currentNode != loc then
      Rcv {arg = parsed_arg_fst; loc; thn = parsed_thn}
    else
      parsed_thn
  | Let {fst = Assoc{loc; arg}; snd; thn} ->
    let parsed_arg = parse_ast arg currentNode in
    let parsed_snd = parse_ast snd currentNode in 
    let parsed_thn = parse_ast thn currentNode in
    if loc = currentNode then 
      Let {binder = parsed_arg; arg = parsed_snd; thn = parsed_thn}
    else 
      Application {funct = Fun {name = "F"; arg = ChoreoVars "X"; body = parsed_thn}; argument = parsed_snd}
  | Let {fst = _; snd = _; thn = _} -> None
  | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
    let parsed_body = parse_ast body currentNode in
    let parsed_arg = parse_ast arg2 currentNode in
    if loc = currentNode then
      Fun {name ; arg = parsed_arg; body = parsed_body}
    else
      Fun {name ; arg = ChoreoVars "X"; body = parsed_body}
  | Fun {name; arg = ChoreoVars x; body} -> 
    let parsed_body = parse_ast body currentNode in
    Fun {name = name ; arg = ChoreoVars x; body = parsed_body}
  | Fun {name = _; arg = _; body = _} -> None
  | Application {funct; argument} -> 
    let parsed_funct = parse_ast funct currentNode in
    let parsed_argument = parse_ast argument currentNode in
      Application {funct = parsed_funct; argument = parsed_argument}
  | ChoreoVars x -> ChoreoVars x
  | Snd _ | Abstraction _ | Comm_S _ | UMinus _ -> None 

(* val parse_ast: expr -> string -> ctrl *)


(* let () = Printf.printf "%s\n\n" (show_ctrl (parse_ast ast "Person2")) *)

let () = SS.iter (fun entity -> 
  let res = parse_ast ast entity in
  let str = "____________________________" ^ entity ^ "___________________________________" in 
  print_endline str;
  Printf.printf "%s\n\n" ( show_ctrl res)
  ) entities