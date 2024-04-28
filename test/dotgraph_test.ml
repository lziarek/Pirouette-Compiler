open OUnit2
open Ast.Interface
open Parsing.Interface

let peq (s : string) (expected_dot : string) =
  let stmts = parse_program (Lexing.from_string s) in
  let generated_dot = dot_graph stmts in
  (generated_dot, expected_dot)

(* let test_declaration_basic _ =
  peq "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;" *)

let new_decl _ = 
  let generated_dot, expected_dot = peq "type x := P1.int" 
  "digraph G {
n0 [label=\"x\"];
n0 -> n1;
n1 [label=\"P1\"];
n1 -> n2;
n2 [label=\"Int\"];

}
" in 
  print_endline "Generated Dot Code:";
  print_endline generated_dot;
  print_endline "Expected Dot Code:";
  print_endline expected_dot;
  assert_equal expected_dot generated_dot

let int_assign _ = 
  let generated_dot, expected_dot = peq "x := P1.5;" 
  "digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"x\"];
n2 [label=\"P1\"];
n2 -> n3;
n3 [label=\"5\"];

}
" in 
  print_endline "Generated Dot Code:";
  print_endline generated_dot;
  print_endline "Expected Dot Code:";
  print_endline expected_dot;
  assert_equal expected_dot generated_dot

let decl_expr _ = 
  let generated_dot, expected_dot = peq "(P1.5, P2.true) : P1.int * P2.bool;" 
  "digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n6;
n1 [label=\"Pair\"];
n1 -> n2;
n1 -> n4;
n2 [label=\"P1\"];
n2 -> n3;
n3 [label=\"5\"];
n4 [label=\"P2\"];
n4 -> n5;
n5 [label=\"true\"];
n6 [label=\"Product\"];
n6 -> n7;
n6 -> n9;
n7 [label=\"P1\"];
n7 -> n8;
n8 [label=\"Int\"];
n9 [label=\"P2\"];
n9 -> n10;
n10 [label=\"Bool\"];

}
" in 
  print_endline "Generated Dot Code:";
  print_endline generated_dot;
  print_endline "Expected Dot Code:";
  print_endline expected_dot;
  assert_equal expected_dot generated_dot

(* let pair_assign _ = peq "pair1 := (P1.5, P2.true);"
let binary_operation _ =
  peq "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
let test_first_pair _ = peq " y := fst(P1.1, P1.2);"
let test_second_pair _ = peq " y := snd(P1.1, P1.2);"
let test_decl_send _ = peq "y : P2.int;\n        y := P1.5 ~> P2;" *)

let suite =
  "Dot Graph Tests"
  >::: [
          "Declaration"
          >::: ["New declaration" >:: new_decl;
                "Int assignment" >:: int_assign;
                "Pair Declaration" >:: decl_expr; 
              ];
        ]

let () = run_test_tt_main suite