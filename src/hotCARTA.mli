(** Implementation of an abstract context-aware ranked tree automaton (CARTA).

  @author Robert Jakob
*)

(** Signature of a CARTA. *)
module type S = sig

  (** The ranked alphabet used for the input symbols. *)
  module RankedAlphabet : HotRankedAlphabet.S

  (** Terms created from the ranked alphabet. These are used in the rule. *)
  module Term : HotTerm.S

  (** Type of a state *)
  type state =
    | SSingle of string * Term.Path.t
    | SMultiple of (string * Term.Path.t) list

  (** A set of states create from the state representation. *)
  module States : HotExtBatSet.S with type elt = state

  (** Distinguishes between drain states or normal
    resulting states. *)
  type state_t =
    | SDrain
    | SStates of state list

  (** A transition rule consists of a current state, an input term,
    a path to the current symbol and a list of resulting states. *)
  type rule = state * Term.t * Term.Path.t * state_t

  (** Module representing a set of rules. *)
  module RuleSet : sig
    include HotExtBatSet.S with type elt = rule

    val string_of: t -> string
  end

  (** Internal type of the CARTA *)
  type t

  (** Create a new automaton based on a set of states,
    a ranked alphabet of input symbols, a set of transition rules
    and an initial state. *)
  val create : RankedAlphabet.t -> States.t -> RuleSet.t -> state -> t

  (** Adds some rules to the given CARTA. *)
  val add_rules : t -> RuleSet.t -> t

  (** Fetches all transitions with some start state. *)
  val get_transitions : t -> state -> RuleSet.t

  (** Returns a string representation of a given state. *)
  val string_of_state : state -> string

  (** Returns a string representation of a given state. *)
  val string_of_rule : rule -> string

  (** String representation of the current CARTA. *)
  val string_of : t -> string

  (** Creates a drain for the transition. *)
  val mkDrain : state_t

  (** Creates simple transition goal states. *)
  val mkStates : state list -> state_t

  (** Create a simple single state. *)
  val mkSingleState : (string * Term.Path.t) -> state

  (** Create a state as a union of several states. *)
  val mkMultipleState : (string * Term.Path.t) list -> state

  (** Creates a multiple state from a list of single ones. *)
  val mkMultipleStateFromSingleList : state list -> state

  (** Union of two states, i.e. respects SSingle and SMultiple *)
  val state_union : state -> state -> state

  (** Checks if two given CARTA are equivalent. *)
  val equal : t -> t -> bool
end

(** Create an instance of a CARTA using a state representation
  and a ranked alphabet representation. *)
module Make : functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) -> S
         with type RankedAlphabet.elt = Elt.t * Type.t
         and type Term.re = Elt.t * Type.t
