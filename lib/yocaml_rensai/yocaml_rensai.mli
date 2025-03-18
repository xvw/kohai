(** Support of Rensai as a YOCaml metadata *)

(** @inline *)
include
  Yocaml.Required.DATA_READER
  with type t = Rensai.Ast.t
   and type 'a eff := 'a Yocaml.Eff.t
   and type ('a, 'b) arr := ('a, 'b) Yocaml.Task.t
   and type extraction_strategy := Yocaml.Metadata.extraction_strategy
