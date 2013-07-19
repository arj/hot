open Batteries

module type S = sig
  type t
  val create : t -> t
  val of_string : string -> t
  val compare : t -> t -> int
  val string_of : t -> string
  val subst : t -> t -> t -> t
  val epsilon : t
end

module Make = functor (S : S) -> struct
  type t = S.t
  let create s = s
  let compare = S.compare
  let string_of = S.string_of
  let of_string = S.of_string
  let subst = S.subst
  let epsilon = S.epsilon
end
