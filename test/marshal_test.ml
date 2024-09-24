open OUnit2
open Http.Send_receive
open Types_test



let test_marshal_unmarshal_int _ =
  let data = 10 in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_float _ =
  let data = 3.14 in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_bool _ =
  let data = true in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_string _ =
  let data = "Hello, world!" in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_list _ =
  let data = [1; 2; 3; 4; 5] in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_color _ =
  let data = Red in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let test_marshal_unmarshal_person _ =
  let data = {
    name = "Ethan";
    age = 20;
    favorite_colors = [Green; Blue];
  } in
  let marshaled = marshal_data data in
  match unmarshal_data marshaled with
  | Ok unmarshaled -> assert_equal data unmarshaled
  | Error msg -> assert_failure msg

let suite =
  "Marshal Test Suite" >::: [
    "test_marshal_unmarshal_int" >:: test_marshal_unmarshal_int;
    "test_marshal_unmarshal_float" >:: test_marshal_unmarshal_float;
    "test_marshal_unmarshal_bool" >:: test_marshal_unmarshal_bool;
    "test_marshal_unmarshal_string" >:: test_marshal_unmarshal_string;
    "test_marshal_unmarshal_list" >:: test_marshal_unmarshal_list;
    "test_marshal_unmarshal_color" >:: test_marshal_unmarshal_color;
    "test_marshal_unmarshal_person" >:: test_marshal_unmarshal_person;
  ]

let () =
  run_test_tt_main suite