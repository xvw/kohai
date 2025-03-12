module Make (D : Generic.SIMPLE_RESOLVER) = struct
  let list (module H : Eff.HANDLER) () =
    let cwd = Global.ensure_supervision (module H) () in
    let file = D.resolver ~cwd in
    let content = Eff.read_file (module H) file in
    let lexbuf = Lexing.from_string content in
    lexbuf
    |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
    |> Kohai_model.Described_item.Set.from_list
  ;;

  let save (module H : Eff.HANDLER) item =
    let cwd = Global.ensure_supervision (module H) () in
    let file = D.resolver ~cwd in
    let items = list (module H : Eff.HANDLER) () in
    let items = Kohai_model.Described_item.Set.push item items in
    let content = Kohai_model.Described_item.Set.dump items in
    let () = Eff.write_file (module H) file content in
    items
  ;;

  let get (module H : Eff.HANDLER) item =
    let items = list (module H : Eff.HANDLER) () in
    Kohai_model.Described_item.Set.find item items
  ;;

  let delete (module H : Eff.HANDLER) item_name =
    let cwd = Global.ensure_supervision (module H) () in
    let file = D.resolver ~cwd in
    let items = list (module H : Eff.HANDLER) () in
    let item = Kohai_model.Described_item.Set.find item_name items in
    match item with
    | Some item when Kohai_model.Described_item.can_be_erased item ->
      let items = Kohai_model.Described_item.Set.remove item_name items in
      let content = Kohai_model.Described_item.Set.dump items in
      let () = Eff.write_file (module H) file content in
      items
    | None | Some _ -> items
  ;;

  let manip_counter f (module H : Eff.HANDLER) item_name =
    let cwd = Global.ensure_supervision (module H) () in
    let file = D.resolver ~cwd in
    let items = list (module H : Eff.HANDLER) () in
    let items = f item_name items in
    let content = Kohai_model.Described_item.Set.dump items in
    let () = Eff.write_file (module H) file content in
    items
  ;;

  let increase = manip_counter Kohai_model.Described_item.Set.increase
  let decrease = manip_counter Kohai_model.Described_item.Set.decrease
end
