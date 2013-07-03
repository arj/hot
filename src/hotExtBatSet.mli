(** In this module, an extended BatSet called ExtBatSet is defined. It provides
  additional useful features.

  @author Robert Jakob
*)

module type S =
  sig
    (** {1 From BatSet, for new functionality, see below.} *)
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val singleton : elt -> t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val sym_diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val disjoint : t -> t -> bool
    val compare_subset : t -> t -> int
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val pop : t -> elt * t
    val enum : t -> elt BatEnum.t
    val backwards : t -> elt BatEnum.t
    val of_enum : elt BatEnum.t -> t
    val print :
      ?first:string ->
      ?last:string ->
      ?sep:string ->
      ('a BatInnerIO.output -> elt -> unit) ->
      'a BatInnerIO.output -> t -> unit
    module Exceptionless :
      sig
        val min_elt : t -> elt option
        val max_elt : t -> elt option
        val choose : t -> elt option
      end
    module Labels :
      sig
        val iter : f:(elt -> unit) -> t -> unit
        val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
        val for_all : f:(elt -> bool) -> t -> bool
        val exists : f:(elt -> bool) -> t -> bool
        val map : f:(elt -> elt) -> t -> t
        val filter : f:(elt -> bool) -> t -> t
        val filter_map : f:(elt -> elt option) -> t -> t
        val partition : f:(elt -> bool) -> t -> t * t
      end
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
      type elt = S.t
      type t = BatSet.Make(S).t
      val empty : t
      val is_empty : t -> bool
      val singleton : elt -> t
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val sym_diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val disjoint : t -> t -> bool
      val compare_subset : t -> t -> int
      val iter : (elt -> unit) -> t -> unit
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val pop : t -> elt * t
      val enum : t -> elt BatEnum.t
      val backwards : t -> elt BatEnum.t
      val of_enum : elt BatEnum.t -> t
      val print :
        ?first:string ->
        ?last:string ->
        ?sep:string ->
        ('a BatInnerIO.output -> elt -> unit) ->
        'a BatInnerIO.output -> t -> unit
      module Exceptionless :
        sig
          val min_elt : t -> elt option
          val max_elt : t -> elt option
          val choose : t -> elt option
        end
      module Labels :
        sig
          val iter : f:(elt -> unit) -> t -> unit
          val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
          val for_all : f:(elt -> bool) -> t -> bool
          val exists : f:(elt -> bool) -> t -> bool
          val map : f:(elt -> elt) -> t -> t
          val filter : f:(elt -> bool) -> t -> t
          val filter_map : f:(elt -> elt option) -> t -> t
          val partition : f:(elt -> bool) -> t -> t * t
        end
      val union_all : t list -> t
      val of_list : elt list -> t
      val as_list : t -> elt list
    end
module StringSet : S with type elt = string
