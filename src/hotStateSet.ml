open Batteries

module type S = sig
  include HotExtBatSet.S

  val create : t -> t
  val string_of : t -> string
  val of_string : string -> t
end

module Make = functor (State : HotState.S) -> struct
  include HotExtBatSet.Make(State)

  let create s = s

  let string_of stateset =
    enum stateset
      |> BatList.of_enum
      |> BatList.map State.string_of
      |> String.concat ","
      |> Printf.sprintf "(%s)"

  let of_string (s : string) =
    BatString.strip ~chars:"()[]{}" s
      |> fun s -> BatString.nsplit s ","
      |> BatList.map State.of_string
      |> BatList.enum
      |> of_enum
end

