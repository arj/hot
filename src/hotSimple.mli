(** Some simple concrete modules of the structures defined in this library.

  @author Robert Jakob
  *)


(** A string and sort based implementation of a ranked element. *)
module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string * HotType.Sort.t


(** StringRankedAlphabet and Sort based terms. *)
module StringSortTerm : HotTerm.S with type re = StringRankedAlphabet.elt


(** A PMRS that is based on strings as its ranked elements and basic sorts as
  types. *)
module StringSortPMRS : HotPMRS.S with
type terminals = StringRankedAlphabet.t and
type nonterminal = StringRankedAlphabet.elt and
type nonterminals = StringRankedAlphabet.t and
type term = StringSortTerm.t and
type Rules.elt = StringRankedAlphabet.elt * string list * StringSortTerm.t option * StringSortTerm.t
