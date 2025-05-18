type t = Kohai_model.Log.t

include Yocaml.Required.DATA_READABLE with type t := t

val normalize : t -> Yocaml.Data.t
