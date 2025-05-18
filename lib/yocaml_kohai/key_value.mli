type 'a t = 'a Kohai_model.Key_value.t

val validate
  :  (Yocaml.Data.t -> 'a Yocaml.Data.Validation.validated_value)
  -> Yocaml.Data.t
  -> 'a t Yocaml.Data.Validation.validated_value

val normalize : ('a -> Yocaml.Data.t) -> 'a t -> Yocaml.Data.t
