(** Implementation of an abstract context-aware ranked tree automaton (CARTA).

  @author Robert Jakob
*)

(** Signature of a CARTA. *)
module type S = sig

  (** Type of a state *)
  type state

  (** Type of an input symbol *)
  type symbol

  (** Type of the input alphabet *)
  type alphabet

  (** Type of a rule. *)
  type rule

  (** Internal type of the CARTA *)
  type t

  (** Create a new automaton based on a set of states,
    a ranked alphabet of input symbols, a set of transition rules
    and an initial state. *)
  val create : alphabet -> state list -> rule list -> state -> t

  (** String representation of the current CARTA. *)
  val string_of : t -> string
end

(** Create an instance of a CARTA using a state representation
  and a ranked alphabet representation. *)
module Make : functor (State : HotState.S) ->
  functor(RA : HotRankedAlphabet.S) -> S
          with type state = State.t
          and type symbol = RA.elt
          and type alphabet = RA.t
