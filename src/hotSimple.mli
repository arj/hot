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

(** The SimpleState module represents a state which is
  represented as a simple string. *)
module SimpleState : HotState.S with type t = string

(** A state represented by a path. *)
(*module PathState : HotState.S with type t = string * RtaTerm.Path.t*)

(** A simple CARTA. *)
module SimpleCARTA : HotCARTA.S
          with type state = SimpleState.t
          and type symbol = StringRankedAlphabet.elt
          and type alphabet = StringRankedAlphabet.t
