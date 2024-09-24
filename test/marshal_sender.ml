open OUnit2
open Lwt.Infix
open Http
open Types_test

let setup_logs () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Lwt.return_unit

let send_data url data =
  Send_receive.send_message ~url ~data >>= function
  | Ok () -> Lwt.return_unit
  | Error msg -> Lwt.return (assert_failure msg)

let test_send_int _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url 10)

let test_send_float _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url 3.14)

let test_send_bool _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url true)

let test_send_string _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url "Hello, world!")

let test_send_list _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url [1; 2; 3; 4; 5])

let test_send_color _ =
  let url = "http://localhost:8080/send" in
  Lwt_main.run (send_data url Red)

let test_send_person _ =
  let url = "http://localhost:8080/send" in
  let data = {
    name = "Ethan";
    age = 20;
    favorite_colors = [Green; Blue];
  } in
  Lwt_main.run (send_data url data)

let suite =
  "Marshal Sender Test Suite" >::: [
    "test_send_int" >:: test_send_int;
    "test_send_float" >:: test_send_float;
    "test_send_bool" >:: test_send_bool;
    "test_send_string" >:: test_send_string;
    "test_send_list" >:: test_send_list;
    "test_send_color" >:: test_send_color;
    "test_send_person" >:: test_send_person;
  ]

let () =
  Lwt_main.run (setup_logs ());
  run_test_tt_main suite