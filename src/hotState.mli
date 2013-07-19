(** This module describes an abstract state of an automaton.

  @author Robert Jakob
*)

(** Module representing the interface for a state. *)
module type S = sig
  type t

  (** Creates a new state directly from its internal
    representation, if known. *)
  val create : t -> t

  (** Creates an internal representation from a string.
    This function must be the inverse of string_of. *)
  val of_string : string -> t

  (** Compares two given states. *)
  val compare : t -> t -> int

  (** Produces a string representation of the given state.
    This function must be the inverse of of_string. *)
  val string_of : t -> string

  (** If the first argument equals the last,
    than the second argument is returned otherwise,
    the last is returned.
    subst qold qnew q = if qold = q then qnew else q *)
  val subst : t -> t -> t -> t

  (** A special empty state, often named epsilon. *)
  val epsilon : t
end

(** Generates a state with an internal state
  representation. This is necessary to create
  tuples of states. *)
module Make : functor (S : S) -> S with type t = S.t
