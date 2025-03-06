module type SIMPLE_RESOLVER = sig
  val resolver : cwd:Path.t -> Path.t
end

module type DESCRIBED_ITEM = sig
  (** Returns the set of item. *)
  val list
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> unit
    -> Kohai_model.Described_item.Set.t

  (** Smartly save into the item set. *)
  val save
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> Kohai_model.Described_item.t
    -> Kohai_model.Described_item.Set.t

  (** Find by his name. *)
  val get
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.t option

  (** Delete by his name. *)
  val delete
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t

  (** Increase by name. *)
  val increase
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t

  (** Decrease by name. *)
  val decrease
    :  ?id:int
    -> body:string
    -> (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.Set.t
end
