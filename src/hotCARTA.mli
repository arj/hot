(** Implementation of an abstract context-aware ranked tree automaton (CARTA).

  @author Robert Jakob
*)

(** Signature of a CARTA. *)
module type S = sig

  (** The ranked alphabet used for the input symbols. *)
  module RankedAlphabet : HotRankedAlphabet.S

  (** Terms created from the ranked alphabet. These are used in the rule. *)
  module Term : HotTerm.S


  (** A state is represented as the function where it came from and
    a path to the variable it represents *)
  type inner_state = string * Term.Path.t

  (** A state, that is represented as a set of function * path tuples. *)
  module State : HotExtBatSet.S with type elt = inner_state

  (** Alias for the state type. *)
  type state = State.t

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

    (** Get all states that are used in the rules. *)
    val get_states: t -> States.t

    (** Get all states in the domain and in the range of the
      rules, separately. *)
    val get_states_dom_ran: t -> States.t * States.t

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

  (** Fetches all rules. *)
  val get_rules : t -> RuleSet.t

  (** Filters rules in a CARTA according to a predicate. *)
  val filter_rules : t -> (rule -> bool) -> t

  (** Returns a string representation of a given state. *)
  val string_of_state : state -> string

  (** Returns a string representation of a given state. *)
  val string_of_rule : rule -> string

  (** String representation of the current CARTA. *)
  val string_of : t -> string

  (** Creates a drain for the transition. *)
  val mk_drain : state_t

  (** Creates simple transition goal states. *)
  val mk_states : state list -> state_t

  (** Create a new state that consists of function * path pairs. *)
  val mk_state : (string * Term.Path.t) list -> state

  (** Create a new state that only consists of one function * path pair. *)
  val mk_state_1 : string * Term.Path.t -> state

  (** Union of a list of states. *)
  val mk_state_union : state list -> state

  (** Checks if two given CARTA are equivalent. *)
  val equal : t -> t -> bool

  (** Disjoint union of two automata, i.e. we expect the automata's
    properties to be disjoint. First argument is the new initial state
    to use for the resulting automaton. *)
  val disjoint_union : state -> t -> t -> t
  
  (** Sets the set of state Q used by the automaton
    to all the states used in the rules and the initial state. *)
  val update_q : t -> t

  (** Checks if the CARTA accepts the given finite term.
    Acceptance is trivial, i.e. if every node in the input tree
    can be given a state consistent to the contexts and transitions
    it accepts.
    Result is either Ok or the path in the term that fails.
  *)
  val accepts : t -> Term.t -> (unit,Term.Path.t) BatResult.t
end

(** Create an instance of a CARTA using a state representation
  and a ranked alphabet representation. *)
module Make : functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) -> S
         with type RankedAlphabet.elt = Elt.t * Type.t
         and type Term.re = Elt.t * Type.t
