open OUnit2
open Parsing.Interface

let peq (s : string) =
  let program = parse_program (Lexing.from_string s) in
  let _ = Prettyprint.Interface.pp_ast Format.str_formatter program in
  let program' = parse_program (Lexing.from_string (Format.flush_str_formatter ())) in
  assert_equal program program'

let test_declaration_basic _ =
  peq "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;"
let new_decl _ = peq "type x := P1.int"
let int_assign _ = peq "x := P1.5;"
let decl_expr _ = peq "(P1.5, P2.true) : P1.int * P2.bool;"
let pair_assign _ = peq "pair1 := (P1.5, P2.true);"
let binary_operation _ =
  peq "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
let test_first_pair _ = peq " y := fst(P1.1, P1.2);"
let test_second_pair _ = peq " y := snd(P1.1, P1.2);"
let test_decl_send _ = peq "y : P2.int;\n        y := P1.5 ~> P2;"

let suite =
  "Pretty print Tests"
  >::: [
         "Declaration"
         >::: [ "Basic statement" >:: test_declaration_basic;
                "New declaration" >:: new_decl;
                "Int assignment" >:: int_assign;
                "Pair Declaration" >:: decl_expr;
                "Pair assignment" >:: pair_assign;
                "Binary operation" >:: binary_operation;
                "Send first pair" >:: test_first_pair;
                "Send second pair" >:: test_second_pair;
                "Declaration with send" >:: test_decl_send;
              ];
       ]

let () = run_test_tt_main suite
