(** The standard signature of a HORS. *)
module type S = sig
  module Rules : HotExtBatSet.S
  type terminals
  type nonterminal
  type nonterminals
  type term
  type rules
  type t

  val create : terminals -> nonterminals -> rules
                -> nonterminal -> t
  val string_of : t -> string
end

(* TODO Term.re should be union type of terminals.elt and nonterminals.elt? *)
(* TODO We need two different terms: one for terminals only and one for both.
  Can this be created on the fly via a term? *)
(* TODO Extend with a wrapper which takes one term and constructs a term
  which distinguishes between the two using a data constructor. *)

module Make = functor(Terminals : HotRankedAlphabet.S) ->
  functor(Nonterminals : HotRankedAlphabet.S with type elt = Terminals.elt) ->
  functor(Term : HotTerm.S with type re = Nonterminals.elt) -> struct

    (* TODO Put Rules signature in S? *)
    module Rules = HotExtBatSet.Make(struct type t = Nonterminals.elt * Term.t * Term.t
                                       let compare = compare end)

    type terminals = Terminals.t
    type nonterminal = Nonterminals.elt 
    type nonterminals = Nonterminals.t
    type term = Term.t
    type rules = Rules.t

    type t = {
      s: terminals;
      n: nonterminals;
      r: rules;
      i: nonterminal;
    }

    let create s n r i = {
      s = s;
      n = n;
      r = r;
      i = i;
    }

    let string_of pmrs =
      let s_string = Terminals.string_of pmrs.s  in
      let n_string = Nonterminals.string_of pmrs.n  in
      let r_string =
        let io = BatIO.output_string () in
        let string_of_rule (n,p,t) = Printf.sprintf "%s %s %s -> %s"
                                       (Nonterminals.string_of_elt n)
                                       ("") (*TODO Vars *)
                                       (Term.string_of p)
                                       (Term.string_of t) in
          Rules.print ~sep:("\n")
            (fun out rule -> BatIO.nwrite out (string_of_rule rule)) io pmrs.r;
          BatIO.close_out io in
      let i_string = Terminals.string_of_elt pmrs.i in
        Printf.sprintf "<%s,%s,\n%s,\n%s>"
          s_string
          n_string
          r_string
          i_string
end
