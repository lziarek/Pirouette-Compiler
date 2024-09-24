open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

let sent_data = ref []

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match uri with
    | "/send" ->
        body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
        sent_data := !sent_data @ [body_str];
        Logs.debug (fun m -> m "Received data: %s" body_str);
        let headers = Header.init_with "Content-Type" "application/octet-stream" in
        let response = Response.make ~status:`OK ~headers () in
        (response, Cohttp_lwt.Body.of_string "Data received")
    | "/receive" ->
        let data =
          match !sent_data with
          | [] -> ""
          | hd :: tl ->
              sent_data := tl;
              Logs.debug (fun m -> m "Sending data: %s" hd);
              hd
        in
        let headers = Header.init_with "Content-Type" "application/octet-stream" in
        let response = Response.make ~status:`OK ~headers () in
        Lwt.return (response, Cohttp_lwt.Body.of_string data)
    | _ ->
        let response = Response.make ~status:`Not_found () in
        Lwt.return (response, Cohttp_lwt.Body.empty)
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = Lwt_main.run server