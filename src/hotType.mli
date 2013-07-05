(** In this module, simple types are defined. Simple types consist of
  a base type combined with an arrow from a base type to a base type.

  For base types [int] and [bool], we can create e.g. the following simple
  types: [int], [bool], [int -> int], [bool -> bool], [(int -> int) -> bool].
  Here, the parentheses only visualize the structure and are not used
  internally.

  A basic type is a sort, which has a fixed base "o".

  @author Robert Jakob
*)

(** The signature of a simple type. *)
module type S =
  sig

    (** The type of the basis. *)
    type base

    (** The type of a simple type. *)
    type t = Base of base | Arrow of t * t

    (** Calculates the order of a given type.
      Let k be a base type, then
      order(k) = 0 and order(t1 -> t2) = max (order t1 + 1) (order t2). *)
    val order : t -> int

    (** Calculates the arity of the given simple type.
      Let k be a base type, then
      arity(k) = 0 and arity(t1 -> t2) = arity(t2) + 1 *)
    val arity : t -> int

    (** Returns a human-readable representation for a simple type. *)
    val string_of : t -> string

    (** Comparing two types. Might just be {! Pervasive.compare}. *)
    val compare : t -> t -> int
  end

(** Constructor for a new simpletype from a given base type representation. See
  [!S] for more information. *)
module Make :
  functor (ABasetype : HotInterfaces.PRINTABLE) ->
    sig
      include S with type base = ABasetype.t
    end

(** Predefined simple type called 'sort'. In a sort, the base type simply is a
  string "o". *)
module Sort :
  sig
    include S with type base = string

    (** The basic string "o" used as basis. *)
    val base : t

    (** Creates a flat sort of length n.
      Example: [create_n 0] produces [o -> o] and
      [create_n 2] produces [o -> o -> o]. *)
    val create_n : int -> t

    module Infix :
    sig
      val o : t
      val ( ^=> ) : t -> t -> t
    end
  end
