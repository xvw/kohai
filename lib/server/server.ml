let json ?(status = `OK) obj =
  let headers = Http.Header.of_list [ "content-type", "application/json" ] in
  let body = Yojson.Safe.to_string obj in
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()
;;

let rensai ?status obj = obj |> Rensai.Json.to_yojson |> json ?status

let server_handler (module H : Eff.HANDLER) _supervised _socket request body =
  let meth = request |> Cohttp.Request.meth in
  match meth with
  | `POST ->
    let body = Eio.Flow.read_all body in
    let () = Logs.app (fun f -> f "Received `%s`" body) in
    (match Eff.handle (module H) (Jsonrpc.run ~services:Services.all body) with
     | Ok result -> rensai ~status:`OK result
     | Error err ->
       rensai ~status:`Internal_server_error (err |> Error.to_rensai))
  | _ ->
    let result = Error.internal_error ~body:"" ()
    and status = `Method_not_allowed in
    rensai ~status (result |> Error.to_rensai)
;;

let setup_logger log_level =
  let header = Logs_fmt.pp_header in
  let () = Fmt_tty.setup_std_outputs () in
  let () = Logs.set_reporter Logs_fmt.(reporter ~pp_header:header ()) in
  Logs.set_level (Some log_level)
;;

let run
      (module H : Eff.HANDLER)
      ?(backlog = 128)
      ?(reuse_addr = true)
      ?(reuse_port = true)
      ?(log_level = Logs.App)
      ~port
      env
  =
  Eio.Switch.run (fun sw ->
    let resource = env#net in
    let supervised_directory = ref None in
    let server =
      Cohttp_eio.Server.make
        ~callback:(server_handler (module H) supervised_directory)
        ()
    in
    let () = setup_logger log_level in
    let () = Logs.app (fun f -> f "Start server on `%d`" port) in
    Cohttp_eio.Server.run
      ~on_error:(fun exn -> Logs.err (fun f -> f "%a" Eio.Exn.pp exn))
      (Eio.Net.listen
         ~reuse_addr
         ~reuse_port
         ~sw
         ~backlog
         resource
         (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)))
      server)
;;
