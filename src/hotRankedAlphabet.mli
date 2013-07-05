(** An implementation of a ranked alphabet.

  A ranked alphabet is a set of symbols with types.

  Assume we have the symbols [zero : nat] and [succ : nat -> nat]
  then [(zero : nat, succ : nat -> nat)] forms a ranked alphabet.

  There is no restriction on the order of the symbols.

  @author Robert Jakob
*)

(** A type signature of an element of a ranked alphabet. *)
module type RANKEDELEMENT =
  sig
    (** The internal type of the ranked element *)
    type t

    (** Comparison function. Might just be {! Pervasive.compare}. *)
    val compare : t -> t -> int

    (** The order (as in higher-order) of the element. *)
    val order : t -> int

    (** The arity of the element,
      e.g. [arity "zero"= 0] and [arity "succ" = 1]. *)
    val arity : t -> int

    (** String representation of the element. Should fit in one line. *)
    val string_of : t -> string
  end

(** Type signature of a ranked alphabet. It behaves just like a set with
  additional order and string_of functions. *)
module type S =
  sig
    include HotExtBatSet.S

    (** The maximum order of the elements of the ranked alphabet. O(n). *)
    val order : t -> int

    (** String representation of the entire alphabet. *)
    val string_of : t -> string

    (** String representation of a single element. *)
    val string_of_elt : elt -> string
  end

(** Creates a new ranked alphabet from an implementation for an
  element. *)
module Make :
  functor (S : RANKEDELEMENT) ->
    sig
      include HotExtBatSet.S with type t = HotExtBatSet.Make(S).t and type elt = S.t

      (** Returns the maximum order in the alphabet.
        Complexity: O(n).
        Returns -1 if the alphabet is empty. *)
      val order : t -> int

      (** Prints a string representation of the ranked alphabet.
        Details are implementation specific. *)
      val string_of : t -> string

      (** Prints a string representation of a ranked element. *)
      val string_of_elt : elt -> string
    end
