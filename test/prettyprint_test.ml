(*
  File: prettyprint_tests.ml
  Date: 04-25-2024
  
  Tests for the pretty print module.
  Read and parse the string representation of a program from Testcases.ml to AST
  pretty print it in formatted code and then parse the result back to check
  if the ASTs are identical.
*)

  open OUnit2
  
  let peq (s : string) =
    let program = Parsing.parse_program (Lexing.from_string s) in
    let pprint_s = Ast_utils.stringify_pprint_choreo_ast program in
    let program' = Parsing.parse_program (Lexing.from_string pprint_s) in
    assert_equal program program'
  
  let suite =
    "Pretty print Tests"
    >::: [
          "Examples"
          >::: [ "testcase1" >:: (fun _ -> peq Testcases.testcase_1);
                  "testcase2" >:: (fun _ -> peq Testcases.testcase_2);
                  "testcase3" >:: (fun _ -> peq Testcases.testcase_3);
                ];
         ]
  
  let () = run_test_tt_main suite