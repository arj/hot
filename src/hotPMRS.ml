(** The standard signature of a HORS. *)
module type S = sig
  type terminals
  type terminal
  type nonterminals
  type term
  type rules
  type t

  val create : terminals -> nonterminals -> rules
                -> terminal -> t
  val string_of : t -> string
end

module Make = functor(Terminals : HotRankedAlphabet.S) ->
  functor(Nonterminals : HotRankedAlphabet.S) ->
  functor(Term : HotTerm.S) -> struct

    type terminals = Terminals.t
    type terminal = Terminals.elt 
    type nonterminals = Nonterminals.t
    type term = Term.t
    type rules = int list

    type t = {
      s: terminals;
      n: nonterminals;
      r: rules;
      i: terminal;
    }

    let create s n r i = {
      s = s;
      n = n;
      r = r;
      i = i;
    }

    let string_of pmrs = failwith "Not yet implemented"
end
