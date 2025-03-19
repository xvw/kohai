(** Operation related to transient logs. *)

(** List all current transient logs. *)
val all : (module Sigs.EFFECT_HANDLER) -> Kohai_model.Transient_log.t list

(** List all current transient logs (and sort them). *)
val list
  :  (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.Transient_log.t list

(** Get a current log by index. *)
val get
  :  (module Sigs.EFFECT_HANDLER)
  -> int
  -> Kohai_model.Transient_log.t option

(** Store missing artifacts (project and sector). *)
val store_missing_artifacts
  :  (module Sigs.EFFECT_HANDLER)
  -> project:string option
  -> sector:string
  -> unit

(** Save a given transient log into a file. *)
val save
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Transient_log.t
  -> Kohai_model.Transient_log.result

(** Record a new transient log. *)
val record
  :  (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Context.t
  -> date_query:Datetime.Query.t option
  -> project:string option
  -> sector:string
  -> label:string
  -> Kohai_model.Transient_log.result

(** Stop recording of a transient log. *)
val stop_record
  :  (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Context.t
  -> index:int
  -> duration:int option
  -> Kohai_model.Transient_log.result

(** Rewrite an existing transient log. *)
val rewrite
  :  (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Context.t
  -> index:int
  -> date_query:Datetime.Query.t option
  -> project:string option
  -> sector:string
  -> label:string
  -> Kohai_model.Transient_log.result

(** Delete an existing transient log. *)
val delete
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> Kohai_model.Transient_log.result

(** Add metadata to a transient log *)
val add_meta
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> key:string
  -> value:string
  -> Kohai_model.Transient_log.result

(** Remove metadata to a transient log *)
val remove_meta
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> key:string
  -> Kohai_model.Transient_log.result

(** Add link to a transient log *)
val add_link
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> key:string
  -> value:Kohai_model.Url.t
  -> Kohai_model.Transient_log.result

(** Remove link to a transient log *)
val remove_link
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> key:string
  -> Kohai_model.Transient_log.result

(** Duplicate a transient log *)
val duplicate
  :  (module Sigs.EFFECT_HANDLER)
  -> index:int
  -> Kohai_model.Transient_log.result
