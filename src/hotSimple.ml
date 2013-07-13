(** This module provides simple concrete implementations
  for the abstract modules provided in this package.

  @author Robert Jakob
*)


module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string *
                                                 HotType.Sort.t =
  HotRankedAlphabet.Make(HotHelper.HotExtBatString)(HotType.Sort)

module StringSortTerm = HotTerm.Make(StringRankedAlphabet)

module StringSortPMRS =
  HotPMRS.Make(StringRankedAlphabet)(StringRankedAlphabet)(StringSortTerm)
