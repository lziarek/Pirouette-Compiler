open OUnit2
open Lwt.Infix
open Http
open Types_test

let setup_logs () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Lwt.return_unit

let receive_and_unmarshal url expected_data =
  Send_receive.receive_message ~url >>= function
  | Ok unmarshaled ->
      Logs.debug (fun m -> m "Unmarshaled data: %s" (Marshal.to_string unmarshaled []));
      assert_equal expected_data unmarshaled;
      Lwt.return_unit
  | Error msg -> Lwt.return (assert_failure msg)

let test_receive_int _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url 10)

let test_receive_float _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url 3.14)

let test_receive_bool _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url true)

let test_receive_string _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url "Hello, world!")

let test_receive_list _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url [1; 2; 3; 4; 5])

let test_receive_color _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (receive_and_unmarshal url Red)

let test_receive_person _ =
  let url = "http://localhost:8080/receive" in
  let expected = {
    name = "Ethan";
    age = 20;
    favorite_colors = [Green; Blue];
  } in
  Lwt_main.run (receive_and_unmarshal url expected)

let suite =
  "Marshal Receiver Test Suite" >::: [
    "test_receive_int" >:: test_receive_int;
    "test_receive_float" >:: test_receive_float;
    "test_receive_bool" >:: test_receive_bool;
    "test_receive_string" >:: test_receive_string;
    "test_receive_list" >:: test_receive_list;
    "test_receive_color" >:: test_receive_color;
    "test_receive_person" >:: test_receive_person;
  ]

let () =
  Lwt_main.run (setup_logs ());
  run_test_tt_main suite