(** Module containing certain helper classes.
  @author Robert Jakob
*)

(** Extension of {!Batteries.BatString} to implement {!HotInterfaces.PRINTABLE}.
  *)
module HotExtBatString = struct
  include BatString

  (** Just the identity function *)
  let string_of t = t
end
