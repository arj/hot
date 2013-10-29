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

  (** Returns a string representation of the automaton. *)
  val string_of : t -> string
end

module Make :
  functor (RA : HotRankedAlphabet.S) ->
    functor (Rules : RULES) ->
      sig
        type state = string
        type symbol = RA.elt
        type rule = Rules.elt
        type t 
        val create : state list -> symbol list -> rule list -> state -> t
        val add_rule : t -> Rules.elt -> t
        val by_q : t -> state -> rule list
        val by_q_a : t -> state -> Rules.symbol -> rule list
        val string_of : t -> string
      end
