(*
  File: testcases.ml
  Date: 04-25-2024
  
  Strings for testing Pirouette modules.
*)

(*Declarations*)
let testcase_1 = 
"_ := let \n
  R.x := [S] S.1 ~> R;\n
  R.x := [S] S.3 ~> R; in \n
  if R.(x>5) \n
  then R[L] ~> S; \n
  S.\"Hello\"\n
  else R[R] ~> S; \n
  S.\"Bye\";\n"

let testcase_2 =
"_ := \n
if R.(3+5 > 2-1)\n
then R[L] ~> S;\n
      let R.res := [S] S.(1,true) ~> R; in R.\"Sent\"\n
else R[R] ~> S;\n
      let R.res := [S] S.(0,false) ~> R; in R.\"why\"\n
;"

let testcase_3 =
"y: P2.int;\n
y := if P1.(3>5)\n
   then P1[L] ~> P2;\n
          P2.5\n
     else P1[R] ~> P2;\n
          P2.9\n
;"