(** Some shared interfaces *)

type jsonrpc_error =
  | Parse_error of { body : string }
  | Invalid_request of
      { body : string
      ; id : int option
      ; error : Rensai.Validation.value_error option
      }
  | Method_not_found of
      { body : string
      ; id : int option
      ; meth : string
      }
  | Invalid_params of
      { body : string
      ; id : int option
      ; error : Rensai.Validation.value_error
      }
  | Internal_error of
      { body : string
      ; id : int option
      ; message : string option
      }
  | Custom_error of
      { body : string
      ; id : int option
      ; code : int
      ; message : string option
      }

(** Describes all the effects a program can propagate. This module
    serves as a requirement for building a Handler. *)
module type EFFECT_REQUIREMENT = sig
  val exists : Path.t -> bool
  val is_file : Path.t -> bool
  val is_dir : Path.t -> bool
end

(** An effect handler is built around a
    {!module-type:EFFECT_REQUIREMENT} to propagate platform-specific
    operations.*)
module type EFFECT_HANDLER = sig
  exception Jsonrpc_exn of jsonrpc_error

  (** [raise error] throws a fixed-error. *)
  val raise : jsonrpc_error -> 'a

  (** [handle_with_error program], handle [program ()] with
      exception. *)
  val handle_with_error : (unit -> 'a) -> ('a, jsonrpc_error) result
end
