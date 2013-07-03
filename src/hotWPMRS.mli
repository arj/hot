(** In this module, a weak pattern-matching higher-order recursion scheme (wPMRS)
  is defined. For reference, please see e.g. Ong, Luke et.al. - Verifying
  Higher-Order Functional Programs with Pattern-Matching Algebraic Data Types.
  POPL2011.

  @author Robert Jakob
*)

(** The standard signature of a WPMRS. *)
module type S = sig

  (** {1 Types} *)

  (** The type of the WPMRS. *)
  type t

  (** The type of the ranked alphabet used internally. *)
  type ra

  (** The type of a higher-order recursion scheme. *)
  type hors

  (** Creates an empty WPMRS. *)
  val create : unit -> t

  (** {1 Transformations} *)

  (** Creates a wPMRS from a HORS. *)
  val from_hors : hors -> t

  (** Creates a wPMRS from a HORSC. *)
  val from_horsc : horsc -> t
    
  (** {1 Utilities} *)

  (** Returns a human-readable representation from a given HORS. *)
  val string_of : t -> string
end
