
(** A string and sort based implementation of a ranked element. *)
module StringRankedAlphabet : HotRankedAlphabet.S with type elt = string * HotTypes.Sort.t