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
  type state = string * Term.Path.t

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

  (** Internal type of the CARTA *)
  type t

  (** Create a new automaton based on a set of states,
    a ranked alphabet of input symbols, a set of transition rules
    and an initial state. *)
  val create : RankedAlphabet.t -> States.t -> rule list -> state -> t

  (** String representation of the current CARTA. *)
  val string_of : t -> string

  (** Creates a drain for the transition. *)
  val mkDrain : state_t

  (** Creates simple transition goal states. *)
  val mkStates : state list -> state_t
end

(** Create an instance of a CARTA using a state representation
  and a ranked alphabet representation. *)
module Make : functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) -> S
         with type RankedAlphabet.elt = Elt.t * Type.t
         and type Term.re = Elt.t * Type.t
