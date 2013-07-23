open Batteries

module type S = sig

  type state

  type symbol

  type alphabet

  type rule

  type t

  val create : alphabet -> state list -> rule list -> state -> t

  val string_of : t -> string
end

module Make = functor (State : HotState.S) ->
  functor(RA : HotRankedAlphabet.S) ->
struct

  type state = State.t

  type symbol = RA.elt

  type alphabet = RA.t

  type rule = int

    type t = {
      s : RA.t;
      qs : State.t list; (* TODO Sets *)
      rules : rule list; (* TODO Sets *)
      q : State.t;
    }

  let create s qs rules q = {
    s = s;
    qs = qs;
    rules = rules;
    q = q;
  }

  let string_of carta =
      let s_string = RA.string_of carta.s  in
      let qs_string = String.concat "," @@ List.map State.string_of carta.qs in
      let q_string = State.string_of carta.q in
        Printf.sprintf "<%s,%s,\n%s,\n%s>"
          s_string
          qs_string
          "Not implemented yet"
          q_string
end
