type t = Kohai_core.Datetime.t

val validate : Yocaml.Data.t -> t Yocaml.Data.Validation.validated_value
val to_yocaml : t -> Yocaml.Datetime.t
val normalize : t -> Yocaml.Data.t
