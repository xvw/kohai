let () =
  let callback = Server.run ~port:8888 in
  Eio_main.run callback
;;
