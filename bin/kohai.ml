open Kohai_core
open Kohai_server

let () =
  Eio_main.run (fun env ->
    let module Handler =
      Kohai_core.Eff.Handler (struct
        let supervised_directory = ref None

        let path_to_eio path =
          let root =
            match Path.is_absolute path with
            | true -> Eio.Path.(Eio.Stdenv.fs env / "/")
            | false -> Eio.Stdenv.cwd env
          in
          List.fold_left Eio.Path.( / ) root (Path.to_list path)
        ;;

        let exists path =
          let p = path_to_eio path in
          match Eio.Path.kind ~follow:true p with
          | `Not_found -> false
          | _ -> true
        ;;

        let is_dir path =
          let p = path_to_eio path in
          Eio.Path.is_directory p
        ;;

        let is_file path =
          let p = path_to_eio path in
          Eio.Path.is_file p
        ;;

        let read_file path =
          let p = path_to_eio path in
          try Eio.Path.load p with
          | _ ->
            (* Maybe improve that case lol. *)
            ""
        ;;

        let create_dir path =
          let p = path_to_eio path in
          try Eio.Path.mkdir ~perm:0o755 p with
          | _ ->
            (* Maybe improve that case lol. *)
            ()
        ;;

        let write_file path content =
          let p = path_to_eio path in
          try
            Eio.Path.save ~append:false ~create:(`Or_truncate 0o775) p content
          with
          | _ -> ()
        ;;

        let append_to_file path content =
          let p = path_to_eio path in
          try
            Eio.Path.save ~append:true ~create:(`If_missing 0o775) p content
          with
          | _ -> ()
        ;;

        let set_supervised_directory v = supervised_directory := v
        let get_supervised_directory () = !supervised_directory
      end)
    in
    Server.run (module Handler) env)
;;
