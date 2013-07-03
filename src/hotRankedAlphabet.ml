open Batteries

module Sort = HotTypes.Sort

module type RANKEDELEMENT = sig
  include BatSet.OrderedType

  val order : t -> int
  val arity : t -> int
  val string_of : t -> string
end

module type S = sig
  include HotExtBatSet.S

  val order : t -> int

  val string_of : t -> string

  val string_of_elt : elt -> string
end

module Make = functor(S : RANKEDELEMENT) -> struct
  include HotExtBatSet.Make(S)

  let order ra =
    fold (fun elt ack -> max ack (S.order elt)) ra (-1)

  let string_of ra =
    let io = BatIO.output_string () in
      print
        ~first:"{"
        ~last:"}"
        ~sep:","
        (fun io elt -> BatIO.nwrite io (S.string_of elt))
        io ra;
      BatIO.close_out io

  let string_of_elt = S.string_of
end

module StringRankedAlphabet : S with type elt = string *
                                                 Sort.t = Make(
    struct
      type name = string
      type t = name * Sort.t
      let compare = compare
      let order (_,t) = Sort.order t
      let arity (_,t) = Sort.arity t
      let string_of (n,t) =
        Printf.sprintf "%s:%s" n (Sort.string_of t)
    end)
