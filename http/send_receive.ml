open H2
open Async

(** [marshal_data data] converts [data] into a string representation.
    Returns: A string containing the marshalled data.
    Note: This function can marshal any OCaml value. *)
let marshal_data data =
  Marshal.to_string data []

(** [unmarshal_data str] converts a string representation back into the original data.
    Returns: The unmarshalled data of type 'a.
    Raises: Failure if the string is not a valid marshalled representation.
    Note: The caller is responsible for knowing the expected type of the unmarshalled data. *)
let unmarshal_data str : 'a =
  Marshal.from_string str 0

(** [send_message connection message] sends [message] over the given [connection].
    The message is sent as a POST request to the root path of the server.
    Returns: A Deferred.t that resolves when the message has been sent.
    Requires: [connection] is an open H2 connection. *)
let send_message connection message =
  let marshalled_message = marshal_data message in
  let request = 
    Request.create
      `POST
      "/"
      ~scheme:"https"
      ~headers:(Headers.of_list [
        ":authority", "compiler.example.com";
        "content-type", "application/octet-stream";
        "content-length", string_of_int (String.length marshalled_message)
      ])
  in
  let request_body = Client.request connection request ~error_handler:(fun _ -> ()) ~response_handler:(fun _ _ -> ()) in
  Body.Writer.write_string request_body marshalled_message;
  Body.Writer.close request_body;
  return ()

(** [receive_message connection] receives a message from the given [connection].
    The message is received as a GET request to the root path of the server.
    Returns: A Deferred.t that resolves to the received message as the original data type.
    If an error occurs, the Deferred.t resolves to an error message.
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
            let marshalled_data = Buffer.contents buffer in
            try
              let unmarshalled_data = unmarshal_data marshalled_data in
              Ivar.fill response_received (Ok unmarshalled_data)
            with
            | Failure msg -> Ivar.fill response_received (Error ("Unmarshalling error: " ^ msg)))
      in
      read_response ()
    | _ ->
      Printf.eprintf "Unexpected response status: %s\n" 
        (Status.to_string response.Response.status);
      Ivar.fill response_received (Error "Unexpected response status")
  in
  let error_handler error =
    Printf.eprintf "Error: %s\n" (H2.Error_code.to_string error);
    Ivar.fill response_received (Error "Connection error")
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


