open OUnit2
open Ast.Local
open Ast.Choreo

(* open Ast.Net *)
open Parsing

(* comment *)

let peq (s : string) (v : 'a) =
  let lexbuf = Lexing.from_string s in
  assert_equal v (parse_program lexbuf)
;;

let test_declarations_basic _ =
  (* peq "var : loc.bool" (Prog [ VarDecl (VarId "var", TLoc (LocId "loc", TBool)) ]);
     peq "fun fn : loc.int -> loc.int" (Prog [FunDecl (FunId "fn", TLoc (LocId "loc", TInt), TLoc (LocId "loc", TInt)) ]);
     peq "loc.var : loc.string" (Prog [ LocVarDecl (LocId "loc", VarId "var", LocId "loc", TString) ]); *)
  peq "type new := unit" (Prog [ TypeDecl (TypId "new", TUnit) ])
;;

let new_decl _ =
  peq "type x := P1.int" (Prog [ TypeDecl (TypId "x", TLoc (LocId "P1", TInt)) ])
;;

let int_assign _ =
  peq
    "x := P1.5;"
    (Prog [ Assign ([ Var (VarId "x") ], LocExpr (LocId "P1", Val (`Int 5))) ])
;;

let decl_expr _ =
  peq
    "(P1.5, P2.true) : P1.int * P2.bool;"
    (Prog
       [ Decl
           ( Pair
               (LocPatt (LocId "P1", Val (`Int 5)), LocPatt (LocId "P2", Val (`Bool true)))
           , TProd (TLoc (LocId "P1", TInt), TLoc (LocId "P2", TBool)) )
       ])
;;

let pair_assign _ =
  peq
    "pair1 := (P1.5, P2.true);"
    (Prog
       [ Assign
           ( [ Var (VarId "pair1") ]
           , Pair
               (LocExpr (LocId "P1", Val (`Int 5)), LocExpr (LocId "P2", Val (`Bool true)))
           )
       ])
;;

let binary_operation _ =
  peq
    "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
    (Prog
       [ Assign
           ( [ Var (VarId "y") ]
           , If
               ( LocExpr
                   ( LocId "P1"
                   , BinOp
                       ( BinOp (Val (`Int 3), Gt, Val (`Int 5))
                       , And
                       , BinOp (Val (`Int 4), Lt, Val (`Int 0)) ) )
               , LocExpr (LocId "P1", Val (`Int 3))
               , LocExpr (LocId "P1", Val (`Int 6)) ) )
       ])
;;

let test_first_pair _ =
  peq
    " y := fst(P1.\"Hello\", P1.\"World\");"
    (Prog
       [ Assign
           ( [ Var (VarId "y") ]
           , Fst
               (Pair
                  ( LocExpr (LocId "P1", Val (`String "Hello"))
                  , LocExpr (LocId "P1", Val (`String "World")) )) )
       ])
;;

let test_second_pair _ =
  peq
    " y := snd(P1.\"Hello\", P1.\"World\");"
    (Prog
       [ Assign
           ( [ Var (VarId "y") ]
           , Snd
               (Pair
                  ( LocExpr (LocId "P1", Val (`String "Hello"))
                  , LocExpr (LocId "P1", Val (`String "World")) )) )
       ])
;;

let test_decl_send _ =
  peq
    "y : P2.int;\n        y := P1.5 [P1] ~> P2;"
    (Prog
       [ Decl (Var (VarId "y"), TLoc (LocId "P2", TInt))
       ; Assign
           ( [ Var (VarId "y") ]
           , Send (LocId "P1", LocExpr (LocId "P1", Val (`Int 5)), LocId "P2") )
       ])
;;

let suite =
  "Parser Tests"
  >::: [ "Declarations"
         >::: [ "Basic Declarations" >:: test_declarations_basic
              ; "New Declarations" >:: new_decl
              ; "Assign test" >:: int_assign
              ; "Pair Assignment" >:: pair_assign
              ; "Pair Declaration " >:: decl_expr
              ; "Binary operations " >:: binary_operation
              ; "Send first of pair" >:: test_first_pair
              ; "Send second of pair" >:: test_second_pair
              ; "Testing declare and send" >:: test_decl_send
              ]
       ]
;;

let () = run_test_tt_main suite
