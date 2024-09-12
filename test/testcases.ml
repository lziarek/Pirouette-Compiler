(*
  File: testcases.ml
  Date: 04-25-2024
  
  Strings for testing Pirouette modules.
*)

(*Declarations*)
let declaration_basic = "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;"
let decl_app_send_func = "send_func : P1.int -> P2.int -> unit;\n y := send_func P1.y P2.y;"
let decl_app_lcl_func = "func1 : P1.string -> P.unit;\n y := let P.y := let z := (left P1.(3 + 1 - 1), right P1.(4 * 2)); in func1 P.\"Hello\"; in y;"
let new_decl = "type x := P1.int"
let decl_expr = "(P1.5, P2.true) : P1.int * P2.bool;"
let decl_send = "y : P2.int;\n y := P1.5 ~> P2;"
let decl_sum_expr = "P1.5 : P1.int + P2.int;"
let lcl_prod_sum = "P.(3) : P.(int + int);\n  P.(3,true) : P.(int * bool);"
let ret_lambda_func = "_ := (fun lambda1 -> P1.5 ~> P2);"

(*Assignments*)
let int_assign = "x := P1.5;"
let pair_assign = "pair1 := (P1.5, P2.true);"
let binary_ops = "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
let choreo_pair = "y := fst(P1.\"Hello\", P1.\"World\");\n z := snd(P1.1, P1.2);"
let lcl_pair = "y := P1.(fst(\"Hello\", \"World\")); z := P1.(snd(\"Hello\", \"World\")); "
let sync_send = "y := let P.y := y; in P1[seller] ~> P2; y;"
let choreo_lr_assign = "left y := right P1.2;\n right x := left P1.3;"
let lcl_lr_assign = "P1.(left y) := P2.(right 2); P1.(right x) := P2.(left 3);"

(*pattern matching*)
let choreo_pat_match =
"y := let P.y := y; in\n
  match P.(5 >= 4 && 5/2 != 3 || 2 <= 2) with\n
    | P.true -> P.2 = 2\n
    | P.false -> P.1 = World\n
    | _ -> ();"
let lcl_pat_match =
"y := P.let y := 3 in\n
  match z with\n
  | _ -> ()\n
;"
