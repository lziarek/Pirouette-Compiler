open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix 

(* Function to marshal data *)
let marshal_data data =
  Marshal.to_string data []

(* Function to unmarshal data *)
let unmarshal_data data_str =
  Marshal.from_string data_str 0

(* Function to send a message *)
let send_message ~url ~data =
  let marshaled_data = marshal_data data in
  let headers = Header.init_with "Content-Type" "application/octet-stream" in
  let body = Cohttp_lwt.Body.of_string marshaled_data in
  Client.post ~headers ~body (Uri.of_string url) >>= fun (resp, body) ->  
  let status = resp |> Response.status |> Code.code_of_status in
  Cohttp_lwt.Body.drain_body body >>= fun () ->  
  if status = 200 then
    Lwt.return_ok ()
  else
    Lwt.return_error ("Failed to send message, status code: " ^ string_of_int status)

(* Function to receive a message *)
let receive_message ~url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let status = resp |> Response.status |> Code.code_of_status in
  if status = 200 then
    body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
    Ok (unmarshal_data body_str)
  else
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    Lwt.return_error ("Failed to receive message, status code: " ^ string_of_int status)

