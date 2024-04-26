(*
  File: testcases.ml
  Date: 04-25-2024
  
  Strings for testing Pirouette modules.
*)

(*Declarations*)
let declaration_basic = "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;"
let decl_app_send_func = "send_func : P1.int -> P2.int -> unit;\n y := send_func P1.y P2.y;"
let new_decl = "type x := P1.int"
let decl_expr = "(P1.5, P2.true) : P1.int * P2.bool;"
let decl_send = "y : P2.int;\n y := P1.5 ~> P2;"
let decl_sum_expr = "P1.5 : P1.int + P2.int;"
let ret_lambda_func = "_ := (fun lambda1 -> P1.5 ~> P2);"

(*Assignments*)
let int_assign = "x := P1.5;"
let pair_assign = "pair1 := (P1.5, P2.true);"
let binary_ops = "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
let first_pair = " y := fst(P1.1, P1.2);"
let second_pair = " y := snd(P1.1, P1.2);"
let sync_send = "y := let P.y := y; in P1[seller] ~> P2; y;"
let choreo_lr_assign = "left y := right P1.2;\n right x := left P1.3;"

(*pattern matching*)
let choreo_pat_match = "y := let P.y := y; in\n  match P.(y > 3) with\n  | P.true -> P.1 = Hello\n  | P.false -> P.1 = World\n  | _ -> ();"