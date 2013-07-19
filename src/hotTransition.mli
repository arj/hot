(** Implementation of a transition of an automaton.

  @author Robert Jakob

*)

(** This type represents the two different types of
  ranges our transition can have. Either it is
  a star range, which represents a drain, or
  it is a list of subsequent states. *)
type 'a range =
  | TSBar of 'a (** Represents the star state (drain). *)
  | TSList of 'a list (** Represents a simple list of subsequent states. *)

(** Module representing the interface for a transition. *)
module type S =
sig
  (** The type of the transition. *)
  type t

  (** Type of a symbol *)
  type symbol

  (** Type of a state. *)
  type state

  (** Comparison function on two transitions. *)
  val compare : t -> t -> int

  (** Create a string representation of a given transition. *)
  val string_of : t -> string

  (** Substitute a state in a given transition. *)
  val subst_state : state -> state -> t -> t

  (** Extract the initial state in a transition. *)
  val state : t -> state

  (** Extract the resulting states from a transition *)
  val states : t -> state range
end

(** Generates a transition module depending on the representation
  of states. *)
module Make :
  functor (RankedAlphabet : HotRankedAlphabet.S) ->
  functor (State : HotState.S) -> S with type state = State.t and
type symbol = RankedAlphabet.elt and
type t = State.t * RankedAlphabet.elt * State.t range
