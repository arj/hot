open Batteries

module type S = sig
  include HotExtBatSet.S

  val order : t -> int

  val string_of_elt : elt -> string

  val string_of : t -> string
end

module Make = functor(Elt : HotInterfaces.ORDEREDPRINTABLE) ->
   functor(Type : HotType.S) ->
struct
  include HotExtBatSet.PairSet(Elt)(Type)

  let order ra =
    fold (fun (elt,tpe) ack -> max ack (Type.order tpe)) ra (-1)

  let string_of_elt (elt,tpe) = Printf.sprintf "%s:%s"
                                  (Elt.string_of elt)
                                  (Type.string_of tpe)
  let string_of ra =
    let io = BatIO.output_string () in
      print
        ~first:"{"
        ~last:"}"
        ~sep:","
        (fun io e -> BatIO.nwrite io @@ string_of_elt e)
        io ra;
      BatIO.close_out io
end
