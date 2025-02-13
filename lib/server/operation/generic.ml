module type SIMPLE_RESOLVER = sig
  val resolver : cwd:Path.t -> Path.t
end

module type DESCRIBED_ITEM = sig
  (** Returns the set of item. *)
  val list
    :  ?body:string
    -> ?id:int
    -> (module Sigs.EFFECT_HANDLER)
    -> unit
    -> Kohai_model.Described_item.Set.t

  (** Smartly save a sector into the item set. *)
  val save
    :  ?body:string
    -> ?id:int
    -> (module Sigs.EFFECT_HANDLER)
    -> Kohai_model.Described_item.t
    -> Kohai_model.Described_item.Set.t

  (** Find a sector by his name. *)
  val get
    :  ?body:string
    -> ?id:int
    -> (module Sigs.EFFECT_HANDLER)
    -> string
    -> Kohai_model.Described_item.t option
end
