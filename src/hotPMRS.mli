(** In this module, a pattern-matching recursion scheme (PMRS) is defined.
  For reference, please see Ong and Ramsay - Verifying higher-order functional
  programs with pattern-matching algebraic data types. POPL2011.

  @author Robert Jakob
*)

(** The standard signature of a HORS. *)
module type S = sig

  (** {1 Types} *)

  (** The type of the ranked alphabet used for the terminal symbols. *)
  type terminals

  (** Type of a terminal symbol *)
  type terminal

  (** The type of the ranked alphabet used for the nonterminal symbols. *)
  type nonterminals

  type term

  type rules

  (** The type of the HORS. *)
  type t

  (** Create a new PMRS with an alphabet of terminals, nonterminals, a set of
    rules and an initial terminal symbol as starting point. *)
  val create : terminals -> nonterminals -> rules
                -> terminal -> t

  (** {1 Predicates} *)

  (** Checks wether the given HORS is safe according to the
    syntactic definition of safety in e.g. Knapik, T. et al. Higher-Order
    Pushdown Trees are easy. FoSSaCS’02. *)
  (*val is_safe : t -> bool*)

  (** {1 Utilities} *)

  (** Returns a human-readable representation from a given HORS. *)
  val string_of : t -> string
end
