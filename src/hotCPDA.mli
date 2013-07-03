(** In this module, a order-n collapsible push down automaton (CPDA) is
  defined. For formal introduction, please see e.g. Hague, M. et al. -
  Collapsible Pushdown Automata and Recursion Schemes.
  
  @author Robert Jakob
*)

(** The standard signature of a CPDA. *)
module type S = sig

  (** {1 Types} *)

  type t
  type rs
  type pda

  (** Creates a new CPDA with default values. *)
  val create : unit -> t

  (** {1 Predicates} *)

  (** Checks if the given CPDA produces the empty language
    when seen as a generator. *)
  val is_empty : t -> bool

  (** {1 Transformations } *)

  (** Transforms the given CPDA into an equi-expressive
    recursion scheme, when the CPDA is seen as a tree generator. *)
  val to_recursion_scheme : t -> rs

  (** A given higher-order push down automaton is transformed into a
    equivalent CPDA, i.e. no collapse operation is used. *)
  val from_pda : pda -> t

  (** {1 Utilities} *)

  (** A human-readable representation of a CPDA. *)
  val string_of : t -> string
end
