(** Sectors are categories (used to classify logs). *)

(** Returns the set of sectors a list. *)
val list
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.Sector.Set.t

(** Smartly save a sector into the sector set. *)
val save
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Sector.t
  -> Kohai_model.Sector.Set.t

(** Find a sector by his name. *)
val get
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.Sector.t option
