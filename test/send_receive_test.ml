open OUnit2
open Lwt.Infix
open Http


let setup_logs () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Lwt.return_unit

let test_send_message _ =
  let url = "http://localhost:8080/send" in
  let data = "Test data" in
  Lwt_main.run (
    setup_logs () >>= fun () ->
    Send_receive.send_message ~url ~data >>= function
    | Ok () -> Lwt.return (assert_bool "Message sent successfully" true)
    | Error msg -> Lwt.return (assert_failure msg)
  )

let test_receive_message _ =
  let url = "http://localhost:8080/receive" in
  let expected_data = "Test data" in  
  Lwt_main.run (
    setup_logs () >>= fun () ->
    Send_receive.receive_message ~url >>= function
    | Ok received_data ->
        assert_equal expected_data received_data;
        Lwt.return_unit
    | Error msg -> Lwt.return (assert_failure msg)
  )

let suite =
  "Send and Receive Tests" >::: [
    "test_send_message" >:: test_send_message;
    "test_receive_message" >:: test_receive_message;
  ]

let () = run_test_tt_main suite