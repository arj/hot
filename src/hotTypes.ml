module type BASETYPE = sig
  type t

  val string_of : t -> string
end

module type S = sig
  type base
  type t =
    | Base of base
    | Arrow of t * t

  val order : t -> int
  val arity : t -> int
  val string_of : t -> string
end

module Make = functor(ABasetype : BASETYPE) -> struct
  type base = ABasetype.t
  type t =
    | Base of base
    | Arrow of t * t

  let rec order t = match t with
    | Base(_) -> 0
    | Arrow(t1,t2) -> max (order t1 + 1) (order t2)

  let rec arity t = match t with
    | Base(_) -> 0
    | Arrow(t1,t2) -> arity(t2) + 1

  let rec string_of s = match s with
    | Base(b) -> ABasetype.string_of b
    | Arrow(t1,t2) ->
        Printf.sprintf
          "(%s -> %s)"
          (string_of t1)
          (string_of t2)
end

module Sort = struct
  include Make(struct type t = string let string_of x = x end)

  let base = Base("o")

  let rec create_n n = match n with
    | n when n < 0 -> raise (Failure "Argument negative")
    | 0 -> base
    | n -> Arrow(base, create_n (n-1))

  module Infix = struct
    let o = base
    let ( ^=> ) s1 s2 = Arrow(s1,s2)
  end
end
