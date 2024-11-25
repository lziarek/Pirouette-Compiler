(*
   File: typcheck_tests.ml
   Date: 10-10-2024

   Tests for the type checking module.
   Read and parse the string representation of a program from Testcases.ml to AST
   Call the type checking function on the AST with the expected type and check if the
   type checking is successful.
*)

open OUnit2
open Typing.Typ_infer

let m : ftv = Ok "dummy info"

(*Const type inference testcases*)
let correct_unit_e = Local.Unit m
let correct_binop_bool_e = Local.BinOp (Val (Int (1, m), m), Eq m, Val (Int (1, m), m), m)
let correct_binop_int_e = Local.BinOp (Val (Int (1, m), m), Plus m, Val (Int (1, m), m), m)
let correct_unop_bool_e = Local.UnOp (Not m, correct_binop_bool_e, m)
let correct_and_bool_e = Local.BinOp (correct_binop_bool_e, And m, correct_unop_bool_e, m)
let correct_unop_int_e = Local.UnOp (Neg m, correct_binop_int_e, m)
let correct_pair_e = Local.Pair (correct_binop_int_e, Val (String ("hello", m), m), m)
let correct_fst = Local.Fst (correct_pair_e, m)
let correct_snd = Local.Snd (correct_pair_e, m)
let correct_left = Local.Left (correct_binop_int_e, m)
let correct_right = Local.Right (correct_binop_int_e, m)

(*Binding local type variables*)
let int_var = Local.BinOp (correct_binop_int_e, Plus m, Var (VarId ("foo", m), m), m)
let bool_var = Local.BinOp (Var (VarId ("foo", m), m), Or m, Val (Bool (false, m), m), m)
let string_var = Local.Pair (correct_binop_int_e, Var (VarId ("foo", m), m), m)

let correct_let_int_e =
  Local.Let (VarId ("foo", m), TInt m, Val (Int (1, m), m), int_var, m)
;;

let correct_let_bool_e =
  Local.Let (VarId ("foo", m), TBool m, Val (Bool (true, m), m), bool_var, m)
;;

let correct_let_str_e =
  Local.Let (VarId ("foo", m), TString m, Val (String ("hello", m), m), string_var, m)
;;

let correct_nested_binding =
  (*foo will be bound to String 'hello' in this case*)
  Local.Let (VarId ("foo", m), TInt m, Val (Int (1, m), m), correct_let_str_e, m)
;;

(*Detect local type errors*)
let incorrect_binop_bool_e =
  Local.BinOp (correct_binop_int_e, Eq m, correct_binop_bool_e, m)
;;

let incorrect_binop_int_e =
  Local.BinOp (correct_binop_int_e, Plus m, correct_binop_bool_e, m)
;;

let incorrect_unop_bool_e = Local.UnOp (Not m, correct_binop_int_e, m)

let incorrect_and_bool_e =
  Local.BinOp (correct_binop_bool_e, And m, correct_binop_int_e, m)
;;

let incorrect_unop_int_e = Local.UnOp (Neg m, correct_binop_bool_e, m)

let incorrect_typ_anno =
  Local.Let (VarId ("foo", m), TBool m, Val (Int (1, m), m), int_var, m)
;;

let incorrect_typ_binding =
  Local.Let (VarId ("foo", m), TBool m, Val (Bool (true, m), m), int_var, m)
;;

(*local patterns*)
let int_p : ftv Local.pattern = Val (Int (1, m), m)
let def_p : ftv Local.pattern = Default m
let var_p : ftv Local.pattern = Var (VarId ("foo", m), m)
let string_p : ftv Local.pattern = Val (String ("hello", m), m)
let pair_p : ftv Local.pattern = Pair (var_p, string_p, m)
let left_int_p : ftv Local.pattern = Left (int_p, m)
let right_def_p : ftv Local.pattern = Right (def_p, m)

(*pattern match of local expr*)
let int_pattn_match =
  Local.Match
    ( Var (VarId ("foo", m), m)
    , [ left_int_p, correct_binop_int_e; right_def_p, correct_unop_int_e ]
    , m )
;;

let mismatched_return_match =
  Local.Match
    ( Var (VarId ("foo", m), m)
    , [ left_int_p, correct_binop_int_e; right_def_p, correct_and_bool_e ]
    , m )
;;

let mismatched_pattn_match =
  Local.Match
    ( Var (VarId ("foo", m), m)
    , [ left_int_p, correct_binop_int_e; int_p, correct_unop_int_e ]
    , m )
;;

let local_expr_typ_eq e expected_t =
  let subst, t = infer_local_expr [] e in
  assert_equal t expected_t;
  assert_equal subst []
;;

let local_expr_typ_failures e failure =
  assert_raises failure (fun _ -> local_expr_typ_eq e (TUnit m))
;;

let local_pattn_typ_eq p expected_ctx expected_t =
  let subst, t, ctx = infer_local_pattern [] p in
  assert_equal t expected_t;
  assert_equal subst [];
  assert_equal ctx expected_ctx
;;

let const_suite =
  "Const type inference tests"
  >::: [ ("Correct infer local unit"
          >:: fun _ -> TUnit m |> local_expr_typ_eq correct_unit_e)
       ; ("Correct infer local bool"
          >:: fun _ -> TBool m |> local_expr_typ_eq correct_binop_bool_e)
       ; ("Correct infer local int"
          >:: fun _ -> TInt m |> local_expr_typ_eq correct_binop_int_e)
       ; ("Correct infer local not bool"
          >:: fun _ -> TBool m |> local_expr_typ_eq correct_unop_bool_e)
       ; ("Correct infer local and bool"
          >:: fun _ -> TBool m |> local_expr_typ_eq correct_and_bool_e)
       ; ("Correct infer local neg int"
          >:: fun _ -> TInt m |> local_expr_typ_eq correct_unop_int_e)
       ; ("Correct infer local pair"
          >:: fun _ -> TProd (TInt m, TString m, m) |> local_expr_typ_eq correct_pair_e)
       ; ("Correct infer local fst pair"
          >:: fun _ -> TInt m |> local_expr_typ_eq correct_fst)
       ; ("Correct infer local snd pair"
          >:: fun _ -> TString m |> local_expr_typ_eq correct_snd)
       ; ("Correct infer local left sum"
          >:: fun _ ->
          TSum (TInt m, TVar (TypId ("T0", m), m), m) |> local_expr_typ_eq correct_left)
       ; ("Correct infer local right sum"
          >:: fun _ ->
          TSum (TVar (TypId ("T0", m), m), TInt m, m) |> local_expr_typ_eq correct_right)
       ]
;;

let local_binding_suite =
  "Local binding type inference tests"
  >::: [ ("Correct infer local int binding"
          >:: fun _ -> TInt m |> local_expr_typ_eq correct_let_int_e)
       ; ("Correct infer local bool binding"
          >:: fun _ -> TBool m |> local_expr_typ_eq correct_let_bool_e)
       ; ("Correct infer local string binding"
          >:: fun _ -> TProd (TInt m, TString m, m) |> local_expr_typ_eq correct_let_str_e
         )
       ; ("Correct nested binding"
          >:: fun _ ->
          TProd (TInt m, TString m, m) |> local_expr_typ_eq correct_nested_binding)
       ]
;;

let correct_pattn_suite =
  "Local pattern type inference tests"
  >::: [ ("Correct infer local int pattern"
          >:: fun _ -> TInt m |> local_pattn_typ_eq int_p [])
       ; ("Correct infer local default pattern"
          >:: fun _ -> TUnit m |> local_pattn_typ_eq def_p [])
       ; ("Correct infer local string pattern"
          >:: fun _ -> TString m |> local_pattn_typ_eq string_p [])
       ; ("Correct infer local pair pattern"
          >:: fun _ ->
          TProd (TVar (TypId ("T0", m), m), TString m, m)
          |> local_pattn_typ_eq pair_p [ "foo", TVar (TypId ("T0", m), m) ])
       ; ("Correct infer local left pattern"
          >:: fun _ ->
          TSum (TInt m, TVar (TypId ("T0", m), m), m) |> local_pattn_typ_eq left_int_p []
         )
       ; ("Correct infer local right pattern"
          >:: fun _ ->
          TSum (TVar (TypId ("T0", m), m), TUnit m, m)
          |> local_pattn_typ_eq right_def_p [])
       ; ("Correct pattern match" >:: fun _ -> TInt m |> local_expr_typ_eq int_pattn_match)
       ]
;;

let incorrect_local_type_suite =
  "Detect local type errors"
  >::: [ ("type error in local binop bool"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_binop_bool_e)
       ; ("type error in local binop int"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_binop_int_e)
       ; ("type error in local unop bool"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_unop_bool_e)
       ; ("type error in local binop bool 2"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_and_bool_e)
       ; ("type error in local unop int"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_unop_int_e)
       ; ("Incorrect type binding"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures incorrect_typ_binding)
       ; ("Incorrect type annotation"
          >:: fun _ ->
          Failure "Type annotation and actual type mismatch"
          |> local_expr_typ_failures incorrect_typ_anno)
       ; ("Unbound variable in local expression"
          >:: fun _ ->
          Failure "Variable not found when inferring expression"
          |> local_expr_typ_failures int_var)
       ; ("Incorrect pattern match - return type mismatch"
          >:: fun _ ->
          Failure "Unification failed" |> local_expr_typ_failures mismatched_return_match
         )
       ; ("Incorrect pattern match - pattern type mismatch"
          >:: fun _ ->
          Failure "Type of patterns are not sum types"
          |> local_expr_typ_failures mismatched_pattn_match)
       ]
;;

let () =
  print_endline "\nConst type inference tests";
  run_test_tt_main const_suite
;;

let () =
  print_endline "\nlocal binding tests";
  run_test_tt_main local_binding_suite
;;

let () =
  print_endline "\nCorrect local pattern type inference tests";
  run_test_tt_main correct_pattn_suite
;;

let () =
  print_endline "\nIncorrect local type inference tests";
  run_test_tt_main incorrect_local_type_suite
;;

let choreo_expr_typ_eq e expected_t =
  let subst, t = infer_choreo_expr [] [] e in
  assert_equal t expected_t;
  assert_equal subst []
;;

let choreo_expr_typ_failures e failure =
  assert_raises failure (fun _ -> choreo_expr_typ_eq e (Choreo.TUnit m))
;;

let choreo_pattern_typ_eq p expected_ctx expected_t =
  let subst, t, ctx = infer_choreo_pattern [] [] p in
  assert_equal t expected_t;
  assert_equal subst [];
  assert_equal ctx expected_ctx
;;

let correct_choreo_unit_e = Choreo.Unit m

let correct_choreo_loc_expr =
  Choreo.LocExpr (Local.LocId ("Alice", m), Local.Val (Local.Int (1, m), m), m)
;;

let correct_choreo_send =
  Choreo.Send
    (Local.LocId ("Alice", m), correct_choreo_loc_expr, Local.LocId ("Bob", m), m)
;;

let correct_choreo_if =
  Choreo.If
    ( Choreo.LocExpr (Local.LocId ("Alice", m), Local.Val (Local.Bool (true, m), m), m)
    , correct_choreo_loc_expr
    , correct_choreo_send
    , m )
;;

let correct_choreo_fundef =
  Choreo.FunDef ([ Choreo.Var (Local.VarId ("foo", m), m) ], correct_choreo_loc_expr, m)
;;

let correct_choreo_funapp =
  Choreo.FunApp (correct_choreo_fundef, correct_choreo_loc_expr, m)
;;

let correct_choreo_pair = Choreo.Pair (correct_choreo_loc_expr, correct_choreo_send, m)
let correct_fst = Choreo.Fst (correct_choreo_pair, m)
let correct_snd = Choreo.Snd (correct_choreo_pair, m)
let correct_left = Choreo.Left (correct_choreo_loc_expr, m)
let correct_right = Choreo.Right (correct_choreo_loc_expr, m)

(*Detect choreo type errors*)
let incorrect_choreo_if_condition =
  Choreo.If
    ( Choreo.LocExpr (Local.LocId ("Alice", m), Local.Val (Local.Int (1, m), m), m)
      (*condition is int instead of bool*)
    , correct_choreo_loc_expr
    , correct_choreo_send
    , m )
;;

let incorrect_choreo_send =
  Choreo.Send
    ( Local.LocId ("Bob", m)
    , (*different/wrong source*)
      correct_choreo_loc_expr
    , Local.LocId ("Charlie", m)
    , m )
;;

(*the first expr should be a function*)
let incorrect_choreo_funapp =
  Choreo.FunApp (correct_choreo_loc_expr, correct_choreo_loc_expr, m)
;;

let choreo_const_suite =
  "Choreo const type inference tests"
  >::: [ ("Correct infer choreo unit"
          >:: fun _ -> Choreo.TUnit m |> choreo_expr_typ_eq correct_choreo_unit_e)
       ; ("Correct infer location expression"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_choreo_loc_expr)
       ; ("Correct infer send"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Bob", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_choreo_send)
       ; ("Correct infer if"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Bob", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_choreo_if)
       ; ("Correct infer function definition"
          >:: fun _ ->
          Choreo.TMap
            ( Choreo.TVar (Choreo.Typ_Id ("T0", m), m)
            , Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
            , m )
          |> choreo_expr_typ_eq correct_choreo_fundef)
       ; ("Correct infer function application"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_choreo_funapp)
       ; ("Correct infer pair"
          >:: fun _ ->
          Choreo.TProd
            ( Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
            , Choreo.TLoc (Local.LocId ("Bob", m), Local.TInt m, m)
            , m )
          |> choreo_expr_typ_eq correct_choreo_pair)
       ; ("Correct infer fst"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_fst)
       ; ("Correct infer snd"
          >:: fun _ ->
          Choreo.TLoc (Local.LocId ("Bob", m), Local.TInt m, m)
          |> choreo_expr_typ_eq correct_snd)
       ; ("Correct infer left"
          >:: fun _ ->
          Choreo.TSum
            ( Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
            , Choreo.TLoc
                (Local.LocId ("dummy_loc", m), Local.TVar (Local.TypId ("T0", m), m), m)
            , m )
          |> choreo_expr_typ_eq correct_left)
       ; ("Correct infer right"
          >:: fun _ ->
          Choreo.TSum
            ( Choreo.TLoc
                (Local.LocId ("dummy_loc", m), Local.TVar (Local.TypId ("T0", m), m), m)
            , Choreo.TLoc (Local.LocId ("Alice", m), Local.TInt m, m)
            , m )
          |> choreo_expr_typ_eq correct_right)
       ]
;;

let incorrect_choreo_type_suite =
  "Detect choreo type errors"
  >::: [ ("Type error in if condition"
          >:: fun _ ->
          Failure "Expected boolean type"
          |> choreo_expr_typ_failures incorrect_choreo_if_condition)
       ; ("Location mismatch in send"
          >:: fun _ ->
          Failure "Source location mismatch"
          |> choreo_expr_typ_failures incorrect_choreo_send)
       ; ("Type error in function application"
          >:: fun _ ->
          Failure "Expected function type"
          |> choreo_expr_typ_failures incorrect_choreo_funapp)
       ]
;;

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

   let () = run_test_ttmain suite *)
