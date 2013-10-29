open Batteries

module type RULES = sig
  type t
  type symbol
  type elt = string * symbol * string list

  val add : elt -> t -> t

  val filter : (elt -> bool) -> t -> t

  val of_list : elt list -> t

  val as_list : t -> elt list

  val string_of : t -> string
end

module type S = sig

  (* The abstract type representing the automaton. *)
  type t

  (* State representation *)
  type state = string

  type symbol

  type rule

  val create : state list -> symbol list -> rule list -> state -> t

  val add_rule : t -> rule -> t

  (** Returns all rules that have domain q *)
  val by_q : t -> state -> rule list

  (** Returns all rules that have domain (q,t), where q is a state and t is a
    symbol.*)
  val by_q_a : t -> state -> symbol -> rule list

  val string_of : t -> string
end

module Make = functor(RA : HotRankedAlphabet.S) -> 
  functor(Rules : RULES) -> struct

  type state = string

  module StateSet = struct
    include HotExtBatSet.Make(struct type t = state let compare = compare end)

    let string_of ss =
      let io = BatIO.output_string () in
        print ~first:"{" ~last:"}" ~sep:","
          (fun out r -> BatIO.nwrite out r) io ss;
        BatIO.close_out io
  end

  type symbol = RA.elt

  type rule = Rules.elt

  type t = {
    states : StateSet.t;
    alphabet : RA.t;
    rules : Rules.t;
    initial : StateSet.elt;
  }

  let create qs sigma delta q_i =
    {
      states = StateSet.of_list qs;
      alphabet = RA.of_list sigma;
      rules = Rules.of_list delta;
      initial = q_i;
    }

  let add_rule auto r =
    { auto with rules = Rules.add r auto.rules}

  (** Returns all rules that have domain q *)
  let by_q auto q = Rules.as_list @@ Rules.filter (fun (q',_,_) -> q = q') auto.rules

  (** Returns all rules that have domain (q,t), where q is a state and t is a
    symbol.*)
  let by_q_a auto q a = Rules.as_list @@ Rules.filter (fun (q',a',_) -> q = q' && a = a') auto.rules

  let string_of auto =
    Printf.sprintf "<%s,%s,%s,%s>"
      (StateSet.string_of auto.states)
      (RA.string_of auto.alphabet)
      (Rules.string_of auto.rules)
      (auto.initial)
end
