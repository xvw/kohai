module Make (D : Generic.SIMPLE_RESOLVER) = struct
  let list ?body ?id (module H : Eff.HANDLER) () =
    let cwd = Global.ensure_supervision ?body ?id (module H) () in
    let file = D.resolver ~cwd in
    let content = Eff.read_file (module H) file in
    let lexbuf = Lexing.from_string content in
    lexbuf
    |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
    |> Kohai_model.Described_item.Set.from_list
  ;;

  let save ?body ?id (module H : Eff.HANDLER) item =
    let cwd = Global.ensure_supervision ?body ?id (module H) () in
    let file = D.resolver ~cwd in
    let items = list ?body ?id (module H : Eff.HANDLER) () in
    let items = Kohai_model.Described_item.Set.push item items in
    let content = Kohai_model.Described_item.Set.dump items in
    let () = Eff.write_file (module H) file content in
    items
  ;;

  let get ?body ?id (module H : Eff.HANDLER) item =
    let items = list ?body ?id (module H : Eff.HANDLER) () in
    Kohai_model.Described_item.Set.find item items
  ;;
end
