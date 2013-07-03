(** This module provides paths which can be used in terms {!HotTerm}.

  Typically, a path is given as pairs of constructor and index, i.e.
  [C.1,A.2,E.5] means a path where the first ctor is [C], then, at 1st position
  there is a [A], at 2nd position a [E] and this path finally points to the 5th
  position inside [E], then.

  @author Robert Jakob
*)

(** Internal path representation. *)
type t =
  | Empty (* The empty path. *)
  | Ele of string * int * t (* A path element consisting of a ctor, position and
                             another path. *)

(** Epsilon represents the empty path. *)
val epsilon : t

(** Returns a string representation for the given path.
  When the optional parameter [?epsilon] is false (default), then
  an empty path results in the empty string, and a path [C.1] results
  in ["C.1"].
  With [?epsilon] beeing true, the empty path results in the string
  ["{epsilon}"].
*)
val string_of : ?epsilon:bool -> t -> string

(** Appends a path to a path. *)
val append : t -> t -> t

(** Reverses a path. *)
val reverse : t -> t

(** Checks if the given path is empty. *)
val is_empty : t -> bool
