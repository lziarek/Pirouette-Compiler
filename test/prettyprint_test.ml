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

let pp_eq (s : string) =
  let program = parse_program (Lexing.from_string s) in
  let _ = pretty_print Format.str_formatter program in
  let program' = parse_program (Lexing.from_string (Format.flush_str_formatter ())) in
  assert_equal program program'

let test_declaration_basic _ = pp_eq declaration_basic
let test_new_decl _ = pp_eq new_decl
let test_int_assign _ = pp_eq int_assign
let test_decl_expr _ = pp_eq decl_expr
let test_pair_assign _ = pp_eq pair_assign
let test_binary_ops _ = pp_eq binary_ops
let test_first_pair _ = pp_eq first_pair
let test_second_pair _ = pp_eq second_pair
let test_decl_send _ = pp_eq decl_send

let suite =
  "Pretty print Tests"
  >::: [
         "Declaration"
         >::: [ "Basic statement" >:: test_declaration_basic;
                "New declaration" >:: test_new_decl;
                "Int assignment" >:: test_int_assign;
                "Pair Declaration" >:: test_decl_expr;
                "Pair assignment" >:: test_pair_assign;
                "Binary operation" >:: test_binary_ops;
                "Send first pair" >:: test_first_pair;
                "Send second pair" >:: test_second_pair;
                "Declaration with send" >:: test_decl_send;
              ];
       ]

let () = run_test_tt_main suite
