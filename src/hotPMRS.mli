(** In this module, a higher-order recursion scheme (HORS) is defined.
  For reference, please see e.g. Kobayashi, Naoki - Higher-Order Model Checking:
  From Theory to Practice.

  @author Robert Jakob
*)

(** The standard signature of a HORS. *)
module type S = sig

  (** {1 Types} *)

  (** The type of the HORS. *)
  type t

  (** The type of the ranked alphabet used internally. *)
  type ra

  (** Creates an empty HORS. *)
  val create : unit -> t

  (** {1 Predicates} *)

  (** Checks wether the given HORS is safe according to the
    syntactic definition of safety in e.g. Knapik, T. et al. Higher-Order
    Pushdown Trees are easy. FoSSaCS’02. *)
  val is_safe : t -> bool

  (** {1 Utilities} *)

  (** Returns a human-readable representation from a given HORS. *)
  val string_of : t -> string
end

