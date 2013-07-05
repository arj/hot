(** This module provides simple concrete implementations
  for the abstract modules provided in this package.

  @author Robert Jakob
*)


module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string *
                                                 HotType.Sort.t =
  HotRankedAlphabet.Make(
    struct
      type name = string
      type t = name * HotType.Sort.t
      let compare = compare
      let order (_,t) = HotType.Sort.order t
      let arity (_,t) = HotType.Sort.arity t
      let string_of (n,t) =
        Printf.sprintf "%s:%s" n (HotType.Sort.string_of t)
    end)
