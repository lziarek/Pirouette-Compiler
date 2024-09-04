open H2
open Async

(** [send_message connection message] sends [message] over the given [connection].
    The message is sent as a POST request to the root path of the server.
    Requires: [connection] is an open H2 connection. *)
let send_message connection message =
  let request = 
    Request.create
      `POST
      "/"
      ~scheme:"https"
      ~headers:(Headers.of_list [
        ":authority", "compiler.example.com";
        "content-type", "text/plain";
        "content-length", string_of_int (String.length message)
      ])
  in
  let request_body = Client.request connection request ~error_handler:(fun _ -> ()) ~response_handler:(fun _ _ -> ()) in
  Body.Writer.write_string request_body message;
  Body.Writer.close request_body;
  return ()

(** [receive_message connection] receives a message from the given [connection].
    The message is received as a GET request to the root path of the server.
    Returns: A promise that resolves to the received message as a string.
    If an error occurs, the promise resolves to an error message.
    Requires: [connection] is an open H2 connection. *)
let receive_message connection =
  let response_received = Ivar.create () in
  let response_handler response response_body =
    match response.Response.status with
    | `OK ->
      let buffer = Buffer.create 1024 in
      let rec read_response () =
        Body.Reader.schedule_read
          response_body
          ~on_read:(fun bigstring ~off ~len ->
            let chunk = Bigstringaf.substring bigstring ~off ~len in
            Buffer.add_string buffer chunk;
            read_response ())
          ~on_eof:(fun () ->
            Ivar.fill response_received (Buffer.contents buffer))
      in
      read_response ()
    | _ ->
      Printf.eprintf "Unexpected response status: %s\n" 
        (Status.to_string response.Response.status);
      Ivar.fill response_received "Error: Unexpected response status"
  in
  let error_handler error =
    Printf.eprintf "Error: %s\n" (H2.Error_code.to_string error);
    Ivar.fill response_received "Error: Connection error"
  in
  let request = 
    Request.create
      `GET
      "/"
      ~scheme:"https"
      ~headers:(Headers.of_list [":authority", "compiler.example.com"])
  in
  let _ = Client.request connection request ~error_handler ~response_handler in
  Ivar.read response_received


