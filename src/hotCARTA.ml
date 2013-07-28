open Batteries

module type S = sig

  module RankedAlphabet : HotRankedAlphabet.S

  module Term : HotTerm.S

  type state =
    | SSingle of string * Term.Path.t
    | SMultiple of (string * Term.Path.t) list

  module States : HotExtBatSet.S with type elt = state

  type state_t =
    | SDrain
    | SStates of state list

  type rule = state * Term.t * Term.Path.t * state_t

  type t

  val create : RankedAlphabet.t -> States.t -> rule list -> state -> t
  val get_transition : t -> state -> rule list

  val string_of_state : state -> string

  val string_of : t -> string

  val mkDrain : state_t

  val mkStates : state list -> state_t
  val mkSingleState : (string * Term.Path.t) -> state
  val mkMultipleState : (string * Term.Path.t) list -> state
  val mkMultipleStateFromSingleList : state list -> state
end

(*TODO Abstract to general tree automaton? *)

module Make = functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) ->
struct

  module RankedAlphabet = HotRankedAlphabet.Make(Elt)(Type)
  module Term = HotTerm.Make(RankedAlphabet)

  type state =
    | SSingle of string * Term.Path.t
    | SMultiple of (string * Term.Path.t) list

  module States = HotExtBatSet.Make(struct type t = state let compare = compare
                                    end)
  type state_t =
    | SDrain
    | SStates of state list

  type rule = state * Term.t * Term.Path.t * state_t

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

  let get_transition carta state =
    List.filter (fun (q,_,_,_) -> q == state) carta.rules

  let string_of_state_internal (prefix, path) =
    if path = Term.Path.epsilon then
      Printf.sprintf "q_%s" prefix
    else
      Printf.sprintf "q_%s:%s" prefix @@ Term.Path.string_of path

  let string_of_state ss = match ss with
    | SSingle(prefix,path) -> string_of_state_internal (prefix,path)
    | SMultiple(ss) ->
        let s = String.concat "," @@ BatList.map string_of_state_internal ss in
          Printf.sprintf "{%s}" s

  let string_of_rule (q,t,p,qs) =
    let q_string = string_of_state q in
    let t_string = Term.string_of ~show_type:false t in
    let p_string = Term.Path.string_of p in
    let qs_string = match qs with
      | SDrain -> "*"
      | SStates(qs') -> Printf.sprintf "(%s)" @@
                        String.concat " " @@ List.map string_of_state qs'
    in
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

  let mkDrain = SDrain
  let mkStates qs = SStates(qs)

  let mkSingleState (prefix,path) = SSingle(prefix,path)
  let mkMultipleState states = SMultiple(states)
  let mkMultipleStateFromSingleList ss =
    let extract state = match state with
      | SSingle(prefix,path) -> (prefix,path)
      | SMultiple(_) -> failwith "Expected single state but got multiple."
    in
      mkMultipleState @@ BatList.map extract ss
end
