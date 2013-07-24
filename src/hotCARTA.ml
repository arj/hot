open Batteries

module type S = sig

  module RankedAlphabet : HotRankedAlphabet.S

  module Term : HotTerm.S

  type state = string * Term.Path.t

  module States : HotExtBatSet.S with type elt = state

  type rule = state * Term.t * Term.Path.t * state list

  type t

  val create : RankedAlphabet.t -> States.t -> rule list -> state -> t

  val string_of : t -> string
end

(*TODO Abstract to general tree automaton? *)

module Make = functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) ->
struct

  module RankedAlphabet = HotRankedAlphabet.Make(Elt)(Type)
  module Term = HotTerm.Make(RankedAlphabet)

  type state = string * Term.Path.t

  module States = HotExtBatSet.Make(struct type t = state let compare = compare
                                    end)

  type rule = state * Term.t * Term.Path.t * state list

    type t = {
      s : RankedAlphabet.t;
      qs : States.t;
      rules : rule list; (* TODO Set? *)
      q : state
    }

  let create s qs rules q = {
    s = s;
    qs = qs;
    rules = rules;
    q = q;
  }

  let string_of_state (prefix, path) =
    if path = Term.Path.epsilon then
      Printf.sprintf "q_%s" prefix
    else
      Printf.sprintf "q_%s:%s" prefix @@ Term.Path.string_of path

  let string_of_rule (q,t,p,qs) =
    let q_string = string_of_state q in
    let t_string = Term.string_of ~show_type:false t in
    let p_string = Term.Path.string_of p in
    let qs_string = String.concat " " @@ List.map string_of_state qs in
      Printf.sprintf "(%s,%s,%s) -> %s"
        q_string
        t_string
        p_string
        qs_string

  let string_of carta =
      let s_string = RankedAlphabet.string_of carta.s  in
      let qs_string =
        let io = BatIO.output_string () in
          States.print ~sep:(",")
            (fun out s -> BatIO.nwrite out (string_of_state s)) io carta.qs;
          BatIO.close_out io in
      let r_string = BatString.concat "\n" @@ BatList.map string_of_rule carta.rules in
      let q_string = string_of_state carta.q in
        Printf.sprintf "<%s,%s,\n%s,\n%s>"
          s_string
          qs_string
          r_string
          q_string
end
