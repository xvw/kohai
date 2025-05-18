type t = Kohai_core.Uuid.t

val validate : Yocaml.Data.t -> t Yocaml.Data.Validation.validated_value
val normalize : t -> Yocaml.Data.t

module Set : sig
  type t = Kohai_core.Uuid.Set.t

  include Yocaml.Required.DATA_READABLE with type t := t

  val normalize : t -> Yocaml.Data.t
end
