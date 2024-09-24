open OUnit2
open Ast.Local
open Ast.Choreo

(* open Ast.Net *)
open Parsing

(* comment *)

let m = ("test", 1)

let peq (s : string) (v : 'a) =
  let lexbuf = Lexing.from_string s in
  assert_equal v (parse_program lexbuf)
;;

let test_declarations_basic _ =
  (* peq "var : loc.bool" (Prog [ VarDecl (VarId "var", TLoc (LocId "loc", TBool)) ]);
     peq "fun fn : loc.int -> loc.int" (Prog [FunDecl (FunId "fn", TLoc (LocId "loc", TInt), TLoc (LocId "loc", TInt)) ]);
     peq "loc.var : loc.string" (Prog [ LocVarDecl (LocId "loc", VarId "var", LocId "loc", TString) ]); *)
  peq "type new := unit" (Prog ([ TypeDecl (TypId ("new", m), TUnit m, m) ], m))
;;

let new_decl _ =
  peq "type x := P1.int" (Prog ([ TypeDecl (TypId ("x", m), TLoc (LocId ("P1", m), TInt m, m), m)], m))
;;

let int_assign _ =
  peq
    "x := P1.5;"
    (Prog ([ Assign ([ Var (VarId ("x", m), m) ], LocExpr (LocId ("P1", m), Val (`Int (5, m), m), m), m) ], m))
;;

let decl_expr _ =
  peq
    "(P1.5, P2.true) : P1.int * P2.bool;"
(Prog
   ([ Decl
       ( Pair
           (LocPatt (LocId ("P1", m), Val (`Int (5, m), m), m), LocPatt (LocId ("P2", m), Val (`Bool (true, m), m), m), m)
       , TProd (TLoc (LocId ("P1", m), TInt m, m), TLoc (LocId ("P2", m), TBool m, m), m), m )
   ], m))

;;

let pair_assign _ =
  peq
    "pair1 := (P1.5, P2.true);"
    (Prog
    ([ Assign
        ( [ Var (VarId ("pair1", m), m) ]
        , Pair
            (LocExpr (LocId ("P1", m), Val (`Int (5, m), m), m), LocExpr (LocId ("P2", m), Val (`Bool (true, m), m), m), m)
        , m)
    ], m))
 
;;

let binary_operation _ =
  peq
    "y := if P1.(3 > 5 && 4 < 0) then P1.3 else P1.6;"
    (Prog
    ([ Assign
        ( [ Var (VarId ("y", m), m) ]
        , If
            ( LocExpr
                ( LocId ("P1", m)
                , BinOp
                    ( BinOp (Val (`Int (3, m), m), Gt m, Val (`Int (5, m), m), m)
                    , And m
                    , BinOp (Val (`Int (4, m), m), Lt m, Val (`Int (0, m), m), m), m), m)
            , LocExpr (LocId ("P1", m), Val (`Int (3, m), m), m)
            , LocExpr (LocId ("P1", m), Val (`Int (6, m), m), m), m)
        , m)
    ], m))
 
;;

let test_first_pair _ =
  peq
    " y := fst(P1.\"Hello\", P1.\"World\");"
    (Prog
    ([ Assign
        ( [ Var (VarId ("y", m), m) ]
        , Fst
            (Pair
               ( LocExpr (LocId ("P1", m), Val (`String ("Hello", m), m), m)
               , LocExpr (LocId ("P1", m), Val (`String ("World", m), m), m), m), m)
        , m)
    ], m))
 
;;

let test_second_pair _ =
  peq
    " y := snd(P1.\"Hello\", P1.\"World\");"
    (Prog
    ([ Assign
        ( [ Var (VarId ("y", m), m) ]
        , Snd
            (Pair
               ( LocExpr (LocId ("P1", m), Val (`String ("Hello", m), m), m)
               , LocExpr (LocId ("P1", m), Val (`String ("World", m), m), m), m), m)
        , m)
    ], m))
 
;;

let test_decl_send _ =
  peq
    "y : P2.int;\n        y := P1.5 [P1] ~> P2;"
    (Prog
    ([ Decl (Var (VarId ("y", m), m), TLoc (LocId ("P2", m), TInt m, m), m)
     ; Assign
        ( [ Var (VarId ("y", m), m) ]
        , Send (LocId ("P1", m), LocExpr (LocId ("P1", m), Val (`Int (5, m), m), m), LocId ("P2", m), m)
        , m)
    ], m))
 
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
