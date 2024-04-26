(*
  File: prettyprint_tests.ml
  Date: 04-25-2024
  
  Tests for the pretty print module.
  Read and parse the string representation of a program from Testcases.ml to AST
  pretty print it in formatted code and then parse the result back to check
  if the ASTs are identical.
*)

open OUnit2
open Parsing.Interface
open Ast.Interface
open Testcases

let peq (s : string) =
  let program = parse_program (Lexing.from_string s) in
  let _ = pretty_print Format.str_formatter program in
  let program' = parse_program (Lexing.from_string (Format.flush_str_formatter ())) in
  assert_equal program program'

let suite =
  "Pretty print Tests"
  >::: [
         "Declaration"
         >::: [ "Basic statement" >:: (fun _ -> peq declaration_basic);
                "New declaration" >:: (fun _ -> peq new_decl);
                "Pair Declaration" >:: (fun _ -> peq decl_expr);
                "Declaration with send" >:: (fun _ -> peq decl_send);
                "Declaration with sum expression" >:: (fun _ -> peq decl_sum_expr);
                "New send func type" >:: (fun _ -> peq decl_app_send_func);
                "Accept return from lambda" >:: (fun _ -> peq ret_lambda_func);
              ];
         "Assignment"
         >::: [ "Int assignment" >:: (fun _ -> peq int_assign);
                "Pair assignment" >:: (fun _ -> peq pair_assign);
                "Binary operation" >:: (fun _ -> peq binary_ops);
                "Send first pair" >:: (fun _ -> peq first_pair);
                "Send second pair" >:: (fun _ -> peq second_pair);
                "Sync send" >:: (fun _ -> peq sync_send);
                "Choreo left and right assign" >:: (fun _ -> peq choreo_lr_assign);
              ];
        "Pattern Matching"
        >::: [ "Choreo pattern matching" >:: (fun _ -> peq choreo_pat_match);
             ];
       ]

let () = run_test_tt_main suite
