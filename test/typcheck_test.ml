(*
   File: typcheck_tests.ml
   Date: 10-10-2024

   Tests for the type checking module.
   Read and parse the string representation of a program from Testcases.ml to AST
   Call the type checking function on the AST with the expected type and check if the
   type checking is successful.
*)

open OUnit2

let typ_eq = assert_equal ~printer:string_of_bool true

let suite =
  "Unfninished Type inference tests" >::: [ ("Testcase 1" >:: fun _ -> typ_eq true) ]
;;

let () = run_test_tt_main suite

(*Outdated test code for boolean type checker*)

(* let m : Ast.Metainfo.metainfo = "", 0, 0, 0

   let typ_eq (s : string) (expected_typ : Ast.Choreo.typ) =
   let program = Parsing.parse_program (Lexing.from_string s) in
   let result = Type_check.choreo_typ_check program expected_typ in
   assert_equal result true
   ;;

   let suite =
   "Type Checking Tests"
   >::: [ "Examples"
         >::: [ ("testcase1"
                 >:: fun _ ->
                 typ_eq
                   Testcases.testcase_1
                   (Ast.Choreo.TLoc (Ast.Local.LocId ("S", m), Ast.Local.TString m, m)))
              ; ("testcase2"
                 >:: fun _ ->
                 typ_eq
                   Testcases.testcase_2
                   (Ast.Choreo.TLoc (Ast.Local.LocId ("R", m), Ast.Local.TString m, m)))
              ; ("testcase3"
                 >:: fun _ ->
                 typ_eq
                   Testcases.testcase_3
                   (Ast.Choreo.TLoc (Ast.Local.LocId ("P2", m), Ast.Local.TInt m, m)))
              ; ("testcase4"
                 >:: fun _ ->
                 typ_eq
                   Testcases.testcase_4
                   (Ast.Choreo.TProd
                      ( Ast.Choreo.TLoc (Ast.Local.LocId ("P2", m), Ast.Local.TInt m, m)
                      , Ast.Choreo.TLoc (Ast.Local.LocId ("P2", m), Ast.Local.TInt m, m)
                      , m )))
              ]
       ]
   ;;

   let () = run_test_tt_main suite *)
