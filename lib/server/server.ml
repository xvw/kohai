let json ?(status = `OK) obj =
  let headers = Http.Header.of_list [ "content-type", "application/json" ] in
  let body = Yojson.Safe.to_string obj in
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()
;;

let rensai ?status obj = obj |> Rensai.Json.to_yojson |> json ?status

let handler _socket _request _body =
  rensai Rensai.Ast.(record [ "message", string "Hello World" ])
;;

let setup_logger log_level =
  let header = Logs_fmt.pp_header in
  let () = Fmt_tty.setup_std_outputs () in
  let () = Logs.set_reporter Logs_fmt.(reporter ~pp_header:header ()) in
  Logs.set_level (Some log_level)
;;

let run
      ?(backlog = 128)
      ?(reuse_addr = true)
      ?(reuse_port = true)
      ?(log_level = Logs.Debug)
      ~port
      env
  =
  Eio.Switch.run (fun sw ->
    let resource = env#net in
    let loopback = Eio.Net.Ipaddr.V4.loopback in
    let stream = `Tcp (loopback, port) in
    let socket =
      Eio.Net.listen ~reuse_addr ~reuse_port ~sw ~backlog resource stream
    in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    let () = setup_logger log_level in
    Cohttp_eio.Server.run
      ~on_error:(fun exn -> Logs.err (fun f -> f "%a" Eio.Exn.pp exn))
      socket
      server)
;;
