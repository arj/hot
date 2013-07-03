(** This module comprises common interfaces used in the whole library.

  @author Robert Jakob
*)

(** The module type {!PRINTABLE} allows easy transformation from a given type
  to a string. *)
module type PRINTABLE = sig
  type t

  (** Returns a human-readable string representation of the given
    object. *)
  val string_of : t -> string
end

