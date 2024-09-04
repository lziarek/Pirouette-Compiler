(* Reads a config file and return a (host, port) tuple *)
let read_config filename =
  let ic = open_in filename in
  let rec read_lines () =
    try
      let line = input_line ic in
      let split = String.split_on_char '=' line in
      match split with
      | [key; value] ->
        let key = String.trim key in
        let value = String.trim value in
        [(key, value)] @ read_lines ()
      | _ -> read_lines () (* Continue reading if the line format is incorrect *)
    with End_of_file ->
      close_in ic;
      []
  in
  let config = read_lines () in
  let host = List.assoc "host" config in
  let port = int_of_string (List.assoc "port" config) in
  (host, port)

(* Prints config information to console*)
let () =
  let (host, port) = read_config "http.config" in
  Printf.printf "Host: %s, Port: %d\n" host port
