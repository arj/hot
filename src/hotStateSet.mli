(** Implementation of an abstract state set implemented using a state.

  @author Robert Jakob
*)

(** Signature of a state set. *)
module type S = sig
  include HotExtBatSet.S

  (** Create a new state set from itself. *)
  val create : t -> t

  (** Create a string representation for the state set. *)
  val string_of : t -> string

  (** Extract a state set from a string representation. *)
  val of_string : string -> t
end

(** Create a concrete state set by instantiation with a concrete state. *)
module Make : functor (State : HotState.S) -> S with type elt = State.t
