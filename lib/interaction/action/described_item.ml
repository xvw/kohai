module type S = sig
  (** Returns the set of item. *)
  val list
    :  (module Sigs.EFFECT_HANDLER)
    -> unit
    -> Kohai_model.Described_item.Set.t

  (** Smartly save into the item set. *)
  val save
    :  (module Sigs.EFFECT_HANDLER)
    -> Kohai_model.Described_item.t
    -> Kohai_model.Described_item.Set.t

  (** Find by his name. *)
  val get
    :  (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.t option

  (** Delete by his name. *)
  val delete
    :  (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t

  (** Increase by name. *)
  val increase
    :  (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t

  (** Decrease by name. *)
  val decrease
    :  (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t

  (** Store missing key. *)
  val store_missing_key : (module Sigs.EFFECT_HANDLER) -> string -> unit

  (** Store missing key (if it exists). *)
  val may_store_missing_key
    :  (module Sigs.EFFECT_HANDLER)
    -> string option
    -> unit
end

module Make (D : sig
    val resolver : cwd:Path.t -> Path.t
  end) : S = struct
  let list (module H : Eff.HANDLER) () =
    let cwd = Global.ensure_supervision (module H) () in
    let file = D.resolver ~cwd in
    let content = Eff.read_file (module H) file in
    let lexbuf = Lexing.from_string content in
    lexbuf
    |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
    |> Kohai_model.Described_item.Set.from_ast_list
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

  let store_missing_key (module H : Eff.HANDLER) item =
    item |> Kohai_model.Described_item.make |> save (module H) |> ignore
  ;;

  let may_store_missing_key (module H : Eff.HANDLER) = function
    | None -> ()
    | Some item -> store_missing_key (module H) item
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
