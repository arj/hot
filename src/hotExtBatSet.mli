(** In this module, an extended BatSet called ExtBatSet is defined. It provides
  additional useful features.

  @author Robert Jakob
*)

module type S =
  sig
    (** {1 From BatSet, for new functionality, see below.} *)
    include BatSet.S

    (** {1 New functionality} *)
    
    (** Provides a possibility to unify more than one ExtBatSet.t given as a
      list. *)
    val union_all : t list -> t

    (** Takes a list of elements and produces a set. *)
    val of_list : elt list -> t

    (** Returns a list of elements. Inverse of [of_list]. *)
    val as_list : t -> elt list
  end
module Make :
  functor (S : BatSet.OrderedType) ->
    sig
      include BatSet.S with type elt = S.t and type t = BatSet.Make(S).t
      val union_all : t list -> t
      val of_list : elt list -> t
      val as_list : t -> elt list
    end

(** String set. *)
module StringSet : S with type elt = string


(** Set of pairs. *)
module PairSet : functor (S1 : BatSet.OrderedType) ->
  functor (S2 : BatSet.OrderedType) -> sig

  include S with type elt = S1.t * S2.t
end
