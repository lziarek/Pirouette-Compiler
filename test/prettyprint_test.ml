open OUnit2
open Parsing.Interface

let peq (s : string) =
  let program = parse_program (Lexing.from_string s) in
  let _ = Prettyprint.Interface.pp_ast Format.str_formatter program in
  let program' = parse_program (Lexing.from_string (Format.flush_str_formatter ())) in
  assert_equal program program'

let test_pretty_print_basic _ =
  peq "y := let P.y := y; in if P.(y > 3) then P.1 else P1.(z) ~> P2.a; y;"

let suite =
  "Pretty print Tests"
  >::: [
         "Pretty print basic"
         >::: [ "Basic statement" >:: test_pretty_print_basic ];
       ]

let () = run_test_tt_main suite
