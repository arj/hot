(** In this module, a order-n push down automaton (PDA) is
  defined. For formal introduction, please see e.g. Maslov, A. N. - Multilevel
  stack automata.

  @author Robert Jakob
*)

(** The standard signature of a PDA. *)
module type S = sig

  (** {1 Types} *)

  (** The internal type of a PDA. *)
  type t

  (** Creates a new PDA with default values. *)
  val create : unit -> t

  (** {1 Predicates} *)

  (** Checks if the given PDA produces the empty language
    when seen as a generator. *)
  val is_empty : t -> bool

  (** {1 Utilities} *)

  (** Returns a human-readable representation from a given PDA. *)
  val string_of : t -> string
end
