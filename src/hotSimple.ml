(** This module provides simple concrete implementations
  for the abstract modules provided in this package.

  @author Robert Jakob
*)


module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string *
                                                 HotTypes.Sort.t =
  HotRankedAlphabet.Make(
    struct
      type name = string
      type t = name * HotTypes.Sort.t
      let compare = compare
      let order (_,t) = HotTypes.Sort.order t
      let arity (_,t) = HotTypes.Sort.arity t
      let string_of (n,t) =
        Printf.sprintf "%s:%s" n (HotTypes.Sort.string_of t)
    end)
