let json ?(status = `OK) obj =
  let headers = Http.Header.of_list [ "content-type", "application/json" ] in
  let body = Yojson.Safe.to_string obj in
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()
;;

(* let rensai ?status obj = obj |> Rensai.Json.to_yojson |> json ?status *)
let handler _supervised _socket _request _body = json `Null

(* let meth = request |> Cohttp.Request.meth in *)
(* match meth with *)
(* | `POST -> *)
(*   let body = Eio.Flow.read_all body in *)
(*   let () = Logs.app (fun f -> f "Received `%s`" body) in *)
(*   let status, result = *)
(*     let open Jsonrpc in *)
(*     services *)
(*       Handler. *)
(*         [ ping *)
(*         ; ensure_supervised_directory supervised *)
(*         ; set_supervised_directory supervised *)
(*         ] *)
(*       body *)
(*   in *)
(*   json ~status result *)
(* | _ -> *)
(*   rensai *)
(*     ~status:`Method_not_allowed *)
(*     (Jsonrpc.internal_error () |> Jsonrpc.error) *)

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
      ?(log_level = Logs.App)
      ~port
      env
  =
  Eio.Switch.run (fun sw ->
    let resource = env#net in
    let supervised_directory = ref None in
    let server =
      Cohttp_eio.Server.make ~callback:(handler supervised_directory) ()
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
