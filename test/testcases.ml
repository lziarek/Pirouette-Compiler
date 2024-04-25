(*
  File: testcases.ml
  Date: 04-25-2024
  
  Strings for testing Pirouette modules.
*)

let declaration_basic = "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;"
let new_decl = "type x := P1.int"
let int_assign = "x := P1.5;"
let decl_expr = "(P1.5, P2.true) : P1.int * P2.bool;"
let pair_assign = "pair1 := (P1.5, P2.true);"
let binary_ops = "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
let first_pair = " y := fst(P1.1, P1.2);"
let second_pair = " y := snd(P1.1, P1.2);"
let decl_send = "y : P2.int;\n y := P1.5 ~> P2;"
