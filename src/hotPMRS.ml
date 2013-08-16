open Batteries

(** The standard signature of a HORS. *)
module type S = sig
  module Rules : sig
    include HotExtBatSet.S
    val string_of : t -> string
  end
  type terminals
  type nonterminal
  type nonterminals
  type term
  type t

  val create : terminals -> nonterminals -> Rules.t
                -> nonterminal -> t

  val disjoint_union : t list -> nonterminal -> t

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
    module Rules = struct
      include HotExtBatSet.Make(struct type t = Nonterminals.elt * string list * Term.t option * Term.t
                                       let compare = compare end)

      let string_of_elt (n,xs,p,t) =
        Printf.sprintf "%s %s %s ==> %s"
          (Nonterminals.string_of_elt ~show_type:false n)
          (String.concat " " xs)
          (BatOption.map_default Term.string_of "" p)
          (Term.string_of t)

      let string_of r =
        let io = BatIO.output_string () in
          print ~sep:("\n")
            (fun out rule -> BatIO.nwrite out (string_of_elt rule)) io r;
          BatIO.close_out io
    end

    type terminals = Terminals.t
    type nonterminal = Nonterminals.elt 
    type nonterminals = Nonterminals.t
    type term = Term.t

    type t = {
      s: terminals;
      n: nonterminals;
      r: Rules.t;
      i: nonterminal;
    }

    let create s n r i = {
      s = s;
      n = n;
      r = r;
      i = i;
    }

    let disjoint_union_two i p1 p2 = {
      s = Terminals.union p1.s p2.s;
      n = Nonterminals.union p1.n p2.n;
      r = Rules.union p1.r p2.r;
      i
    }

    let disjoint_union ps i =
      BatList.reduce (disjoint_union_two i) ps 

    let string_of pmrs =
      let s_string = Terminals.string_of pmrs.s  in
      let n_string = Nonterminals.string_of pmrs.n  in
      let r_string = Rules.string_of pmrs.r in
      let i_string = Terminals.string_of_elt pmrs.i in
        Printf.sprintf "<%s,%s,\n%s,\n%s>"
          s_string
          n_string
          r_string
          i_string
end
