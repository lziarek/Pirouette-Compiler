(*
   File: testcases.ml
   Date: 04-25-2024

   Strings for testing Pirouette modules.
*)

let testcase_1 =
  "left _ := let \n\n\
   R.x := [S] left S.1 ~> R;\n\n\
   R.x := [S] right S.3 ~> R; in \n\n\
   if R.(x> fst 5) \n\n\
   then R[L] ~> S; \n\n\
   S.\"Hello\"\n\n\
   else R[R] ~> S; \n\n\
   S.\"Bye\";\n"
;;

let testcase_2 =
  "right (_,_) := \n\n\
   if R.(3+5 >= 2-1 || snd 3 != left 3)\n\n\
   then R[L] ~> S;\n\n\
   let R.res := [S] (x,y) ~> R; in R.\"Sent\"\n\n\
   else R[R] ~> S;\n\n\
   let R.res := [S] S.(0,false) ~> R; in R.\"why\";\n"
;;

let testcase_3 =
  "type x := P2.int;\n\
   y : P2.int;\n\n\
   y := if P1.(3*2 < 5/2 && right 3 = 3)\n\n\
   then P1[L] ~> P2;\n\n\
   P2.5\n\n\
   else P1[R] ~> P2;\n\n\
   ()\n\
   ;"
;;

let testcase_4 =
  "y := if P1.(3*2 < 5/2 && 3 = 3)\n\n\
   then P1[L] ~> P2;\n\n\
   fst P2.5\n\n\
   else P1[R] ~> P2;\n\n\
   snd P2.5;\n"
;;

let choreo_typs =
  "type choreo_sum_typ := P1.int + P2.int;\n\
  \ \n\
   type choreo_unit := unit;\n\n\
   type choreo_prod_typ := P1.bool * P2.bool;\n\
  \ \n\
   type choreo_map_typ := P1.bool -> P2.bool;\n"
;;

let local_typs =
  "type local_unit := P1.unit;\n\
  \ \n\
   type local_string := P1.string;\n\n\
   type local_prod_typ := P1.(int*string);\n\n\
   type local_sum_typ := P1.(int+string);\n"
;;

let choreo_fundef =
  "foo := fun a -> (a, a);\n\n\
   _ := let P := foo P.3; in\n\n\
  \  if R.(x<=-5 && not false) \n\n\
  \  then R[L] ~> S; \n\n\
  \  S.\"Hello\"\n\n\
  \  else R[R] ~> S; \n\n\
  \  S.();\n"
;;

let choreo_pat_match =
  "y := let P.y := y; in\n\nmatch P.(5 >= 4 && 5/2 != 3 || 2 <= 2) with\n\n|_->();\n"
;;

let lcl_pat_match =
  "y := P.let y := 3 in\n\nmatch (x,z) with\n\n|(true,_)->(1,\"None\");\n"
;;

let lcl_pat_match_2 =
  "y := P.let y := 3 in\n\nmatch (x,z) with\n\n|(left 1,right \"None\")->();\n"
;;
