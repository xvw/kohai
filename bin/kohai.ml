open Kohai_server

let () =
  let callback = Server.run ~port:8888 in
  Eio_main.run (fun env ->
    let module Handler =
      Kohai_core.Eff.Handler (struct
        let exists _ = true
        let is_file _ = true
        let is_dir _ = true
        let set_supervised_directory _ = ()
        let get_supervised_directory () = None
      end)
    in
    callback (module Handler) env)
;;
