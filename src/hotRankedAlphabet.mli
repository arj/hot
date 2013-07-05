(** An implementation of a ranked alphabet.

  A ranked alphabet is a set of symbols with arities (or sorts).

  @author Robert Jakob
*)

module Sort :
  sig
    type base = string
    type t = HotTypes.Sort.t = Base of base | Arrow of t * t
    val order : t -> int
    val arity : t -> int
    val string_of : t -> string
    val base : t
    val create_n : int -> t
    module Infix : sig val o : t val ( ^=> ) : t -> t -> t end
  end
module type RANKEDELEMENT =
  sig
    type t
    val compare : t -> t -> int
    val order : t -> int
    val arity : t -> int
    val string_of : t -> string
  end
module type S =
  sig
    include HotExtBatSet.S
    val order : t -> int
    val string_of : t -> string
    val string_of_elt : elt -> string
  end
module Make :
  functor (S : RANKEDELEMENT) ->
    sig
      include HotExtBatSet.S with type t = HotExtBatSet.Make(S).t and type elt = S.t

      (** Returns the maximum order in the alphabet.
        Complexity: O(n).
        Return -1 if the alphabet is empty. *)
      val order : t -> int

      (** Prints a string representation of the ranked alphabet.
        Details are implementation specific. *)
      val string_of : t -> string

      (** Prints a string representation of a ranked element. *)
      val string_of_elt : elt -> string
    end

module StringRankedAlphabet : S with type elt = string * Sort.t
