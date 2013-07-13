(** In this module, a pattern-matching recursion scheme (PMRS) is defined.
  For reference, please see Ong and Ramsay - Verifying higher-order functional
  programs with pattern-matching algebraic data types. POPL2011.

  @author Robert Jakob
*)

(** The standard signature of a PMRS. *)
module type S = sig

  module Rules : HotExtBatSet.S

  (** {1 Types} *)

  (** The type of the ranked alphabet used for the terminal symbols. *)
  type terminals

  (** Type of a nonterminal symbol *)
  type nonterminal

  (** The type of the ranked alphabet used for the nonterminal symbols. *)
  type nonterminals

  type term

  type rules

  (** The type of the HORS. *)
  type t

  (** Create a new PMRS with an alphabet of terminals, nonterminals, a set of
    rules and an initial terminal symbol as starting point. *)
  val create : terminals -> nonterminals -> rules
                -> nonterminal -> t

  (** {1 Predicates} *)

  (** Checks wether the given HORS is safe according to the
    syntactic definition of safety in e.g. Knapik, T. et al. Higher-Order
    Pushdown Trees are easy. FoSSaCS’02. *)
  (*val is_safe : t -> bool*)

  (** {1 Utilities} *)

  (** Returns a human-readable representation from a given HORS. *)
  val string_of : t -> string
end

(** Creates a new module based on an implementation for
  terminals, nonterminals and a term. *)
module Make :
  functor (Terminals : HotRankedAlphabet.S) ->
    functor
      (Nonterminals : HotRankedAlphabet.S with type elt = Terminals.elt) ->
      functor
        (Term : HotTerm.S with type re = Nonterminals.elt) ->
        sig
          module Rules : HotExtBatSet.S with type elt = Nonterminals.elt * Term.t * Term.t
          type terminals = Terminals.t
          type nonterminal = Nonterminals.elt
          type nonterminals = Nonterminals.t
          type term = Term.t
          type rules = Rules.t
          type t
          val create : terminals -> nonterminals -> rules -> nonterminal -> t
          val string_of : t -> string
        end
