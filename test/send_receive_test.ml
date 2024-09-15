open OUnit2
open Lwt.Infix
open Http


let test_send_message _ =
  let url = "http://localhost:8080/send" in
  let data = "Test data" in
  Lwt_main.run (
    Send_receive.send_message ~url ~data >>= function
    | Ok () -> Lwt.return (assert_bool "Message sent successfully" true)
    | Error msg -> Lwt.return (assert_failure msg)
  )

let test_receive_message _ =
  let url = "http://localhost:8080/receive" in
  Lwt_main.run (
    Send_receive.receive_message ~url >>= function
    | Ok received_data ->
        let expected_data = "Received data" in
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