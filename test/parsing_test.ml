open OUnit2
open Parsing.Interface

(* comment *)

let peq (s : string) (v : 'a) =
  let lexbuf = Lexing.from_string s in
  assert_equal v (parse_program lexbuf)

let test_declarations_basic _ =
  (* peq "var : loc.bool" (Prog [ VarDecl (VarId "var", TLoc (LocId "loc", TBool)) ]);
  peq "fun fn : loc.int -> loc.int" (Prog [FunDecl (FunId "fn", TLoc (LocId "loc", TInt), TLoc (LocId "loc", TInt)) ]);
  peq "loc.var : loc.string" (Prog [ LocVarDecl (LocId "loc", VarId "var", LocId "loc", TString) ]); *)
  peq "type new := unit" (Prog [ TypeDecl (VarId "new", TUnit) ])

let suite =
  "Parser Tests"
  >::: [
         "Declarations"
         >::: [ "Basic Declarations" >:: test_declarations_basic ];
       ]

let () = run_test_tt_main suite
