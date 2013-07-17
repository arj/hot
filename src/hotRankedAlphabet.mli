(** An implementation of a ranked alphabet.

  A ranked alphabet is a set of symbols with types.

  Assume we have the symbols [zero : nat] and [succ : nat -> nat]
  then [(zero : nat, succ : nat -> nat)] forms a ranked alphabet.

  There is no restriction on the order of the symbols.

  @author Robert Jakob
*)

(** Type signature of a ranked alphabet. It behaves just like a set with
  additional order and string_of functions. *)
module type S =
  sig
    include HotExtBatSet.S

    (** The maximum order of the elements of the ranked alphabet. O(n). *)
    val order : t -> int

    (** String representation of a single element.
      The default value of the optional parameter show_type is true.
      It describes whether the type is printed.
    *)
    val string_of_elt : ?show_type:bool -> elt -> string

    (** String representation of the entire alphabet. *)
    val string_of : t -> string

    (** The order of a given element. *)
    val order_of_elt : elt -> int

    (** The arity of a given element. *)
    val arity_of_elt : elt -> int
  end

(** Creates a new ranked alphabet from an implementation for an
  element. *)
module Make :
  functor(Elt : HotInterfaces.ORDEREDPRINTABLE) ->
  functor(Type : HotType.S) ->
    sig
      include HotExtBatSet.S with type t = HotExtBatSet.PairSet(Elt)(Type).t and type elt = (Elt.t * Type.t)

      (** Returns the maximum order in the alphabet.
        Complexity: O(n).
        Returns -1 if the alphabet is empty. *)
      val order : t -> int

      (** Prints a string representation of a ranked element. *)
      val string_of_elt : ?show_type:bool -> elt -> string

      (** Prints a string representation of the ranked alphabet.
        Details are implementation specific. *)
      val string_of : t -> string

      (** The order of a given element. *)
      val order_of_elt : elt -> int

      (** The arity of a given element. *)
      val arity_of_elt : elt -> int
    end
