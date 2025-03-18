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
end

module Make (A : Action.Described_item.S) = struct
  let list = A.list
  let save = A.save
  let get = A.get
  let delete = A.delete
end
