open Batteries

module type S = sig
  include BatSet.S

  val union_all : t list -> t

  val of_list : elt list -> t

  val as_list : t -> elt list
end

module Make = functor(S : BatSet.OrderedType) -> struct
  include BatSet.Make(S)

  let union_all ras = BatList.fold_right union ras empty

  let of_list elts =
    of_enum (BatList.enum elts)

  let as_list set =
    BatList.of_enum (enum set)
end

module StringSet = Make(String)
