open Batteries

module type S = sig

  module RankedAlphabet : HotRankedAlphabet.S

  module Term : HotTerm.S

  type inner_state = string * Term.Path.t

  module InnerStates : HotExtBatSet.S with type elt = inner_state

  type state =
    | SSingle of inner_state
    | SMultiple of InnerStates.t

  module States : HotExtBatSet.S with type elt = state

  type state_t =
    | SDrain
    | SStates of state list

  type rule = state * Term.t * Term.Path.t * state_t

  module RuleSet : sig
    include HotExtBatSet.S with type elt = rule

    val string_of: t -> string
  end

  type t

  val create : RankedAlphabet.t -> States.t -> RuleSet.t -> state -> t
  val add_rules : t -> RuleSet.t -> t
  val get_transitions : t -> state -> RuleSet.t

  val string_of_state : state -> string
  val string_of_rule : rule -> string

  val string_of : t -> string

  val mk_drain : state_t

  val mk_states : state list -> state_t
  val mk_single_state : (string * Term.Path.t) -> state
  val mk_multiple_state : (string * Term.Path.t) list -> state
  val mk_multiple_state_from_single_list : state list -> state

  val state_union : state -> state -> state

  val equal : t -> t -> bool
end

(*TODO Abstract to general tree automaton? *)

module Make = functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) ->
struct

  module RankedAlphabet = HotRankedAlphabet.Make(Elt)(Type)
  module Term = HotTerm.Make(RankedAlphabet)

  type inner_state = string * Term.Path.t

  module InnerStates = HotExtBatSet.Make(struct type t = inner_state let compare = compare end)

  type state =
    | SSingle of inner_state
    | SMultiple of InnerStates.t

  module States = HotExtBatSet.Make(struct type t = state let compare = compare
                                    end)
  type state_t =
    | SDrain
    | SStates of state list

  let string_of_state_internal (prefix, path) =
    if path = Term.Path.epsilon then
      Printf.sprintf "q_%s" prefix
    else
      Printf.sprintf "q_%s:%s" prefix @@ Term.Path.string_of path

  let string_of_state ss = match ss with
    | SSingle(prefix,path) -> string_of_state_internal (prefix,path)
    | SMultiple(ss) ->
        let io = BatIO.output_string () in
          InnerStates.print ~first:("{") ~last:("}") ~sep:(",")
            (fun out r -> BatIO.nwrite out (string_of_state_internal r)) io ss;
          BatIO.close_out io

  type rule = state * Term.t * Term.Path.t * state_t

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

  module RuleSet = struct
    include HotExtBatSet.Make(struct type t = rule let compare = compare end)

    let string_of rs =
      let io = BatIO.output_string () in
        print ~sep:(",")
          (fun out r -> BatIO.nwrite out (string_of_rule r)) io rs;
          BatIO.close_out io 
  end

  type t = {
    s : RankedAlphabet.t;
    qs : States.t;
    rules : RuleSet.t;
    q : state
  }

  let create s qs rules q = {
    s = s;
    qs = qs;
    rules = rules;
    q = q;
  }

  let add_rules carta new_rules =
    { carta with rules = RuleSet.union carta.rules new_rules }

  let get_transitions carta state =
    RuleSet.filter (fun (q,_,_,_) -> q = state) carta.rules

  let string_of carta =
      let s_string = RankedAlphabet.string_of carta.s  in
      let qs_string =
        let io = BatIO.output_string () in
          States.print ~sep:(",")
            (fun out s -> BatIO.nwrite out (string_of_state s)) io carta.qs;
          BatIO.close_out io in
      let r_string = BatString.concat "\n" @@ BatList.map string_of_rule @@ RuleSet.as_list carta.rules in
      let q_string = string_of_state carta.q in
        Printf.sprintf "<%s,%s,\n%s,\n%s>"
          s_string
          qs_string
          r_string
          q_string

  let mk_drain = SDrain
  let mk_states qs = SStates(qs)

  let mk_single_state (prefix,path) = SSingle(prefix,path)

  let mk_multiple_state states =
    let state_set = InnerStates.of_list states in
      SMultiple(state_set)

  let mk_multiple_state_from_single_list ss =
    let ss = BatList.sort_unique Pervasives.compare ss in
    let extract state = match state with
      | SSingle(prefix,path) -> (prefix,path)
      | SMultiple(_) -> failwith "Expected single state but got multiple."
    in
      match ss with
        | [] -> failwith "Empty state list"
        | [s] -> s
        | _ -> mk_multiple_state @@ BatList.map extract ss

  let state_union q1 q2 = match q1, q2 with
    | SSingle(f1,p1), SSingle(f2,p2) -> SMultiple(InnerStates.of_list [(f1,p1);(f2,p2)])
    | SMultiple(lst), SSingle(f2,p2) -> SMultiple(InnerStates.add (f2,p2) lst)
    | SSingle(f1,p1), SMultiple(lst) ->SMultiple(InnerStates.add (f1,p1) lst)
    | SMultiple(lst1), SMultiple(lst2) -> SMultiple(InnerStates.union lst1 lst2)

  let equal c1 c2 =
    RankedAlphabet.equal c1.s c2.s &&
    c1.qs = c2.qs &&
    c1.rules = c2.rules &&
    c1.q = c2.q
end
