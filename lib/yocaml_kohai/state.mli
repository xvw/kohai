type t = Kohai_model.State.t

include Yocaml.Required.DATA_READABLE with type t := t

val validate : Yocaml.Data.t -> t Yocaml.Data.Validation.validated_value
val normalize : t -> Yocaml.Data.t
