open OUnit2
open Ast.Interface
open Parsing.Interface

let peq (s : string) (expected_dot : string) =
  let stmts = parse_program (Lexing.from_string s) in
  let generated_dot = dot_graph stmts in
  (generated_dot, expected_dot)

(*Declarations*)  
let declaration_basic _ =
  let generated_dot, expected_dot = peq "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;" 
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"Let\"];
n2 -> n3 ;
n2 -> n7;
n3 [label=\"Assign\"];
n3 -> n4;
n3 -> n6;
n4 [label=\"P\"];
n4 -> n5;
n5 [label=\"y\"];
n6 [label=\"y\"];
n7 [label=\"If\"];
n7 -> n8;
n7 -> n13;
n7 -> n15;
n8 [label=\"P\"];
n8 -> n9;
n9 [label=\"BinOp\"];
n9 -> n10;
n9 -> n11;
n9 -> n12;
n10 [label=\"y\"];
n11 [label=\"3\"];
n12 [label=\">\"];
n13 [label=\"P\"];
n13 -> n14;
n14 [label=\"1\"];
n15 [label=\"Let\"];
n15 -> n16 ;
n15 -> n22;
n16 [label=\"Assign\"];
n16 -> n17;
n16 -> n19;
n17 [label=\"P2\"];
n17 -> n18;
n18 [label=\"a\"];
n19 [label=\"Send: P2\"];
n19 -> n20;
n20 [label=\"P1\"];
n20 -> n21;
n21 [label=\"z\"];
n22 [label=\"y\"];

}
" in 
    assert_equal expected_dot generated_dot

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
  (* print_endline "Generated Dot Code:";
  print_endline generated_dot;
  print_endline "Expected Dot Code:";
  print_endline expected_dot; *)
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
  assert_equal expected_dot generated_dot

let decl_send _ =
  let generated_dot, expected_dot = peq "y : P2.int;\n y := P1.5 ~> P2;"
"digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"P2\"];
n2 -> n3;
n3 [label=\"Int\"];

n4 [label=\"Assign\"];
n4 -> n5;
n4 -> n6;
n5 [label=\"y\"];
n6 [label=\"Send: P2\"];
n6 -> n7;
n7 [label=\"P1\"];
n7 -> n8;
n8 [label=\"5\"];

}
" in 
  assert_equal expected_dot generated_dot

let decl_sum_expr _ =
  let generated_dot, expected_dot = peq "P1.5 : P1.int + P2.int;"
"digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n3;
n1 [label=\"P1\"];
n1 -> n2;
n2 [label=\"5\"];
n3 [label=\"Sum\"];
n3 -> n4;
n3 -> n6;
n4 [label=\"P1\"];
n4 -> n5;
n5 [label=\"Int\"];
n6 [label=\"P2\"];
n6 -> n7;
n7 [label=\"Int\"];

}
" in 
  assert_equal expected_dot generated_dot

let lcl_prod_sum _ =
  let generated_dot, expected_dot = peq "P.(3) : P.(int + int);\n  P.(3,true) : P.(int * bool);"
"digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n3;
n1 [label=\"P\"];
n1 -> n2;
n2 [label=\"3\"];
n3 [label=\"P\"];
n3 -> n4;
n4 [label=\"Sum\"];
n4 -> n5;
n4 -> n6;
n5 [label=\"Int\"];
n6 [label=\"Int\"];

n7 [label=\"Decl\"];
n7 -> n8;
n7 -> n12;
n8 [label=\"P\"];
n8 -> n9;
n9 [label=\"Pair\"];
n9 -> n10;
n9 -> n11;
n10 [label=\"3\"];
n11 [label=\"true\"];
n12 [label=\"P\"];
n12 -> n13;
n13 [label=\"Product\"];
n13 -> n14;
n13 -> n15;
n14 [label=\"Int\"];
n15 [label=\"Bool\"];

}
" in 
  assert_equal expected_dot generated_dot

let ret_lambda_func _ =
  let generated_dot, expected_dot = peq "_ := (fun lambda1 -> P1.5 ~> P2);"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"Default\"];
n2 [label=\"FunDef: lambda1\"];
n2 -> n3;
n3 [label=\"Send: P2\"];
n3 -> n4;
n4 [label=\"P1\"];
n4 -> n5;
n5 [label=\"5\"];

}
" in 
  assert_equal expected_dot generated_dot

let decl_app_send_func _ = 
  let generated_dot, expected_dot = peq "send_func : P1.int -> P2.int -> unit;\n y := send_func P1.y P2.y;"
"digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"send_func\"];
n2 [label=\"Send\"];
n2 -> n3;
n2 -> n5;
n3 [label=\"P1\"];
n3 -> n4;
n4 [label=\"Int\"];
n5 [label=\"Send\"];
n5 -> n6;
n5 -> n8;
n6 [label=\"P2\"];
n6 -> n7;
n7 [label=\"Int\"];
n8 [label=\"()\"];

n9 [label=\"Assign\"];
n9 -> n10;
n9 -> n11;
n10 [label=\"y\"];
n11 [label=\"FunApp\"];
n11 -> n12;
n11 -> n16;
n12 [label=\"FunApp\"];
n12 -> n13;
n12 -> n14;
n13 [label=\"send_func\"];
n14 [label=\"P1\"];
n14 -> n15;
n15 [label=\"y\"];
n16 [label=\"P2\"];
n16 -> n17;
n17 [label=\"y\"];

}
" in 
  assert_equal expected_dot generated_dot

let decl_app_lcl_func _ = 
  let generated_dot, expected_dot = peq "func1 : P1.string -> P.unit;\n y := let P.y := let z := (left P1.(3 + 1 - 1), right P1.(4 * 2)); in func1 P.\"Hello\"; in y;"
"digraph G {
n0 [label=\"Decl\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"func1\"];
n2 [label=\"Send\"];
n2 -> n3;
n2 -> n5;
n3 [label=\"P1\"];
n3 -> n4;
n4 [label=\"String\"];
n5 [label=\"P\"];
n5 -> n6;
n6 [label=\"()\"];

n7 [label=\"Assign\"];
n7 -> n8;
n7 -> n9;
n8 [label=\"y\"];
n9 [label=\"Let\"];
n9 -> n10 ;
n9 -> n36;
n10 [label=\"Assign\"];
n10 -> n11;
n10 -> n13;
n11 [label=\"P\"];
n11 -> n12;
n12 [label=\"y\"];
n13 [label=\"Let\"];
n13 -> n14 ;
n13 -> n32;
n14 [label=\"Assign\"];
n14 -> n15;
n14 -> n16;
n15 [label=\"z\"];
n16 [label=\"Pair\"];
n16 -> n17;
n16 -> n26;
n17 [label=\"Left\"];
n17 -> n18;
n18 [label=\"P1\"];
n18 -> n19;
n19 [label=\"BinOp\"];
n19 -> n20;
n19 -> n24;
n19 -> n25;
n20 [label=\"BinOp\"];
n20 -> n21;
n20 -> n22;
n20 -> n23;
n21 [label=\"3\"];
n22 [label=\"1\"];
n23 [label=\"+\"];
n24 [label=\"1\"];
n25 [label=\"-\"];
n26 [label=\"Right\"];
n26 -> n27;
n27 [label=\"P1\"];
n27 -> n28;
n28 [label=\"BinOp\"];
n28 -> n29;
n28 -> n30;
n28 -> n31;
n29 [label=\"4\"];
n30 [label=\"2\"];
n31 [label=\"*\"];
n32 [label=\"FunApp\"];
n32 -> n33;
n32 -> n34;
n33 [label=\"func1\"];
n34 [label=\"P\"];
n34 -> n35;
n35 [label=\"Hello\"];
n36 [label=\"y\"];

}
" in 
  assert_equal expected_dot generated_dot

(*Assignments*)  
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
  assert_equal expected_dot generated_dot

let pair_assign _ = 
  let generated_dot, expected_dot = peq "pair1 := (P1.5, P2.true);"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"pair1\"];
n2 [label=\"Pair\"];
n2 -> n3;
n2 -> n5;
n3 [label=\"P1\"];
n3 -> n4;
n4 [label=\"5\"];
n5 [label=\"P2\"];
n5 -> n6;
n6 [label=\"true\"];

}
" in
  assert_equal expected_dot generated_dot

let binary_ops _ = 
  let generated_dot, expected_dot = peq "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"If\"];
n2 -> n3;
n2 -> n14;
n2 -> n16;
n3 [label=\"P1\"];
n3 -> n4;
n4 [label=\"BinOp\"];
n4 -> n5;
n4 -> n9;
n4 -> n13;
n5 [label=\"BinOp\"];
n5 -> n6;
n5 -> n7;
n5 -> n8;
n6 [label=\"3\"];
n7 [label=\"5\"];
n8 [label=\">\"];
n9 [label=\"BinOp\"];
n9 -> n10;
n9 -> n11;
n9 -> n12;
n10 [label=\"4\"];
n11 [label=\"0\"];
n12 [label=\"<\"];
n13 [label=\"&&\"];
n14 [label=\"P1\"];
n14 -> n15;
n15 [label=\"3\"];
n16 [label=\"P1\"];
n16 -> n17;
n17 [label=\"6\"];

}
" in
  assert_equal expected_dot generated_dot

let choreo_pair _ = 
  let generated_dot, expected_dot = peq "y := fst(P1.\"Hello\", P1.\"World\");\n z := snd(P1.1, P1.2);"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"Fst\"];
n2 -> n3;
n3 [label=\"Pair\"];
n3 -> n4;
n3 -> n6;
n4 [label=\"P1\"];
n4 -> n5;
n5 [label=\"Hello\"];
n6 [label=\"P1\"];
n6 -> n7;
n7 [label=\"World\"];

n8 [label=\"Assign\"];
n8 -> n9;
n8 -> n10;
n9 [label=\"z\"];
n10 [label=\"Snd\"];
n10 -> n11;
n11 [label=\"Pair\"];
n11 -> n12;
n11 -> n14;
n12 [label=\"P1\"];
n12 -> n13;
n13 [label=\"1\"];
n14 [label=\"P1\"];
n14 -> n15;
n15 [label=\"2\"];

}
" in
  assert_equal expected_dot generated_dot

let lcl_pair _ = 
  let generated_dot, expected_dot = peq "y := P1.(fst(\"Hello\", \"World\")); z := P1.(snd(\"Hello\", \"World\")); "
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"P1\"];
n2 -> n3;
n3 [label=\"Fst\"];
n3 -> n4;
n4 [label=\"Pair\"];
n4 -> n5;
n4 -> n6;
n5 [label=\"Hello\"];
n6 [label=\"World\"];

n7 [label=\"Assign\"];
n7 -> n8;
n7 -> n9;
n8 [label=\"z\"];
n9 [label=\"P1\"];
n9 -> n10;
n10 [label=\"Snd\"];
n10 -> n11;
n11 [label=\"Pair\"];
n11 -> n12;
n11 -> n13;
n12 [label=\"Hello\"];
n13 [label=\"World\"];

}
" in
  assert_equal expected_dot generated_dot

let sync_send _ = 
  let generated_dot, expected_dot = peq "y := let P.y := y; in P1[seller] ~> P2; y;"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"Let\"];
n2 -> n3 ;
n2 -> n7;
n3 [label=\"Assign\"];
n3 -> n4;
n3 -> n6;
n4 [label=\"P\"];
n4 -> n5;
n5 [label=\"y\"];
n6 [label=\"y\"];
n7 [label=\"Sync: P1[seller] -> P2\"];
n7 -> n8;
n8 [label=\"y\"];

}
" in
  assert_equal expected_dot generated_dot

let choreo_lr_assign _ = 
  let generated_dot, expected_dot = peq "left y := right P1.2;\n right x := left P1.3;"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n3;
n1 [label=\"Left\"];
n1 -> n2;
n2 [label=\"y\"];
n3 [label=\"Right\"];
n3 -> n4;
n4 [label=\"P1\"];
n4 -> n5;
n5 [label=\"2\"];

n6 [label=\"Assign\"];
n6 -> n7;
n6 -> n9;
n7 [label=\"Right\"];
n7 -> n8;
n8 [label=\"x\"];
n9 [label=\"Left\"];
n9 -> n10;
n10 [label=\"P1\"];
n10 -> n11;
n11 [label=\"3\"];

}
" in
  assert_equal expected_dot generated_dot

let lcl_lr_assign _ = 
  let generated_dot, expected_dot = peq "P1.(left y) := P2.(right 2); P1.(right x) := P2.(left 3);"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n4;
n1 [label=\"P1\"];
n1 -> n2;
n2 [label=\"Left\"];
n2 -> n3;
n3 [label=\"y\"];
n4 [label=\"P2\"];
n4 -> n5;
n5 [label=\"Right\"];
n5 -> n6;
n6 [label=\"2\"];

n7 [label=\"Assign\"];
n7 -> n8;
n7 -> n11;
n8 [label=\"P1\"];
n8 -> n9;
n9 [label=\"Right\"];
n9 -> n10;
n10 [label=\"x\"];
n11 [label=\"P2\"];
n11 -> n12;
n12 [label=\"Left\"];
n12 -> n13;
n13 [label=\"3\"];

}
" in
  assert_equal expected_dot generated_dot

(*pattern matching*)
let choreo_pat_match _ = 
  let generated_dot, expected_dot = 
  peq "y := let P.y := y; in\n
  match P.(5 >= 4 && 5/2 != 3 || 2 <= 2) with\n
    | P.true -> P.2 = 2\n
    | P.false -> P.1 = World\n
    | _ -> ();"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"Let\"];
n2 -> n3 ;
n2 -> n7;
n3 [label=\"Assign\"];
n3 -> n4;
n3 -> n6;
n4 [label=\"P\"];
n4 -> n5;
n5 [label=\"y\"];
n6 [label=\"y\"];
n7 [label=\"Match\"];
n7 -> n8;
n7 -> n28 n36 n44;
n8 [label=\"P\"];
n8 -> n9;
n9 [label=\"BinOp\"];
n9 -> n10;
n9 -> n23;
n9 -> n27;
n10 [label=\"BinOp\"];
n10 -> n11;
n10 -> n15;
n10 -> n22;
n11 [label=\"BinOp\"];
n11 -> n12;
n11 -> n13;
n11 -> n14;
n12 [label=\"5\"];
n13 [label=\"4\"];
n14 [label=\">=\"];
n15 [label=\"BinOp\"];
n15 -> n16;
n15 -> n20;
n15 -> n21;
n16 [label=\"BinOp\"];
n16 -> n17;
n16 -> n18;
n16 -> n19;
n17 [label=\"5\"];
n18 [label=\"2\"];
n19 [label=\"/\"];
n20 [label=\"3\"];
n21 [label=\"!=\"];
n22 [label=\"&&\"];
n23 [label=\"BinOp\"];
n23 -> n24;
n23 -> n25;
n23 -> n26;
n24 [label=\"2\"];
n25 [label=\"2\"];
n26 [label=\"<=\"];
n27 [label=\"||\"];
n28 [label=\"Case\"];
n28 -> n29;
n28 -> n31;
n29 [label=\"P\"];
n29 -> n30;
n30 [label=\"true\"];
n31 [label=\"P\"];
n31 -> n32;
n32 [label=\"BinOp\"];
n32 -> n33;
n32 -> n34;
n32 -> n35;
n33 [label=\"2\"];
n34 [label=\"2\"];
n35 [label=\"=\"];
n36 [label=\"Case\"];
n36 -> n37;
n36 -> n39;
n37 [label=\"P\"];
n37 -> n38;
n38 [label=\"false\"];
n39 [label=\"P\"];
n39 -> n40;
n40 [label=\"BinOp\"];
n40 -> n41;
n40 -> n42;
n40 -> n43;
n41 [label=\"1\"];
n42 [label=\"World\"];
n43 [label=\"=\"];
n44 [label=\"Case\"];
n44 -> n45;
n44 -> n46;
n45 [label=\"Default\"];
n46 [label=\"()\"];

}
" in
  assert_equal expected_dot generated_dot

let lcl_pat_match _ = 
  let generated_dot, expected_dot = 
  peq "y := P.let y := 3 in\n
  match z with\n
  | _ -> ()\n
;"
"digraph G {
n0 [label=\"Assign\"];
n0 -> n1;
n0 -> n2;
n1 [label=\"y\"];
n2 [label=\"P\"];
n2 -> n3;
n3 [label=\"Let: y\"];
n3 -> n4;
n3 -> n5;
n4 [label=\"3\"];
n5 [label=\"Match\"];
n5 -> n6;
n5 -> n7;
n6 [label=\"z\"];
n7 [label=\"Case\"];
n7 -> n8;
n7 -> n9;
n8 [label=\"Default\"];
n9 [label=\"()\"];

}
" in
  assert_equal expected_dot generated_dot

let suite =
  "Dot Graph Tests"
  >::: [
          "Declaration"
          >::: ["Basic Declaration" >:: declaration_basic;
                "New declaration" >:: new_decl;
                "Pair Declaration" >:: decl_expr; 
                "Declaration with send" >:: decl_send;
                "Declaration with sum expression" >:: decl_sum_expr;
                "Declaration of local prod & sum typ" >:: lcl_prod_sum;
                "Accept return from lambda" >:: ret_lambda_func;
                "New send func type" >:: decl_app_send_func;
                "New local function"  >:: decl_app_lcl_func;
              ];
          "Assignment"
          >:::  ["Int assignment" >:: int_assign;
                  "Pair assignment" >:: pair_assign;
                  "Binary operation" >:: binary_ops;
                  "Assign choreo pair" >:: choreo_pair;
                  "Assign local pair" >:: lcl_pair;
                  "Sync send" >:: sync_send;
                  "Choreo left and right assign" >:: choreo_lr_assign;
                  "Local left and right assign" >:: lcl_lr_assign;
              ];
          "Pattern Matching"
          >:::  ["Choreo pattern matching" >:: choreo_pat_match;
                  "Local pattern matching" >:: lcl_pat_match;
                ];
        ]

let () = run_test_tt_main suite