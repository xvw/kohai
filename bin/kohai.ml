open Kohai_core
open Kohai_server

let bin = Sys.argv.(0)
let version = "dev"

module Make_handler
    (Env : sig
       val env : Eio_unix.Stdenv.base
     end)
    () =
struct
  let env = Env.env

  include Kohai_core.Eff.Handler (struct
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

      let delete_file path =
        let p = path_to_eio path in
        try Eio.Path.unlink p with
        | _ -> ()
      ;;

      let delete_dir ?(recursive = false) path =
        let p = path_to_eio path in
        try
          if recursive
          then Eio.Path.rmtree ~missing_ok:true p
          else Eio.Path.rmdir p
        with
        | _ -> ()
      ;;

      let now () = Eio.Time.now env#clock

      let datetime_from_float time =
        let Unix.{ tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } =
          Unix.localtime time
        in
        let time = tm_hour, tm_min, tm_sec in
        Datetime.from_unix ~time ~year:tm_year ~month:tm_mon ~day:tm_mday ()
      ;;

      let set_supervised_directory v = supervised_directory := v
      let get_supervised_directory () = !supervised_directory
    end)
end

let run () =
  Eio_main.run (fun env ->
    let module Handler =
      Make_handler
        (struct
          let env = env
        end)
        ()
    in
    Server.run (module Handler) env)
;;

let run_t =
  let open Cmdliner in
  let expr = Term.(const run $ const ()) in
  let doc = "Run the server (using stdin/stdout)" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info expr
;;

let all =
  let open Cmdliner in
  let doc =
    "Kohai is a very simple (but opinionated) timetracker for my personal usage"
  in
  let info = Cmd.info bin ~version ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ run_t ]
;;

let () = exit @@ Cmdliner.Cmd.eval all
