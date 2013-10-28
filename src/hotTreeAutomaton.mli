module type S = sig

  (* The abstract type representing the automaton. *)
  type t

  (* State representation *)
  type state

  type symbol

  type rule = state * symbol * state list

  val create : state list -> symbol list -> rule list -> state -> t

  val add_rule : t -> rule -> t

  val remove_rule : t -> rule -> t

  val union : t -> t -> t

  val intersection : t -> t -> t

  (** Returns all rules that have domain q *)
  val by_q : t -> state -> rule

  (** Returns all rules that have domain (q,t), where q is a state and t is a
    symbol.*)
  val by_q_a : t -> state -> symbol -> rule

  (** Checks if the tree automaton is valid. The exact definition
    of validity is implementation dependant. *)
  val is_valid : t -> bool

  (** Checks if the given automaton is deterministic or not.
    The exact definition of determinism is implementation dependant. *)
  val is_deterministic : t -> bool

  val string_of_rule : rule -> string

  val string_of : t -> string
end
