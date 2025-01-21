open Kohai_server

let () =
  let callback = Server.run ~port:8888 in
  Eio_main.run (fun env ->
    let module Handler = Kohai_core.Eff.Handler (struct end) in
    callback (module Handler) env)
;;
