open Batteries

type 'a range =
  | TSBar of 'a
  | TSList of 'a list

(*TODO Move range into S? *)

module type S = sig
  type t
  type symbol
  type state

  val compare : t -> t -> int

  val string_of : t -> string

  val subst_state : state -> state -> t -> t

  val state : t -> state

  val states : t -> state range
end

module Make = functor(RankedAlphabet : HotRankedAlphabet.S) ->
functor(State : HotState.S) -> struct
  type state = State.t
  type symbol = RankedAlphabet.elt
  type t = state * symbol * state range

  let compare = compare

  let subst_state qold qnew t =
    let subst = State.subst qold qnew in
      match t with
        | (q0,s,TSBar(q1)) -> (subst q0, s, TSBar(subst q1))
        | (q0,s,TSList(qs)) -> (subst q0, s, TSList(List.map subst qs))

  let string_of (q,s,qs) =
    let string_of_range r = match r with
      | TSBar(state) -> Printf.sprintf "%s..." (State.string_of state)
      | TSList(states) ->
          List.map State.string_of states |> String.concat "," |>
            Printf.sprintf "[%s]"
    in
      Printf.sprintf "(%s,%s) -> %s"
        (State.string_of q)
        (RankedAlphabet.string_of_elt s)
        (string_of_range qs)

  let state (q,_,_) = q

  let states (_,_,qs) = qs
end
