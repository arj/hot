open Batteries

module type S = sig

  module RankedAlphabet : HotRankedAlphabet.S

  module Term : HotTerm.S

  type inner_state = string * Term.Path.t

  module State : HotExtBatSet.S with type elt = inner_state

  type state = State.t

  module States : HotExtBatSet.S with type elt = state

  type state_t =
    | SDrain
    | SStates of state list

  type rule = state * Term.t * Term.Path.t * state_t

  module RuleSet : sig
    include HotExtBatSet.S with type elt = rule

    val get_states: t -> States.t

    val get_states_dom_ran: t -> States.t * States.t

    val string_of: t -> string
  end

  type t

  val create : RankedAlphabet.t -> States.t -> RuleSet.t -> state -> t
  val add_rules : t -> RuleSet.t -> t
  val get_transitions : t -> state -> RuleSet.t
  val get_rules : t -> RuleSet.t
  val filter_rules : t -> (rule -> bool) -> t

  val string_of_state : state -> string
  val string_of_rule : rule -> string

  val string_of : t -> string

  val mk_drain : state_t

  val mk_states : state list -> state_t

  val mk_state : (string * Term.Path.t) list -> state
  val mk_state_1 : string * Term.Path.t -> state
  val mk_state_union : state list -> state

  val equal : t -> t -> bool

  val disjoint_union : state -> t -> t -> t

  val update_q : t -> t
  val drain_close : t -> t

  val accepts : t -> Term.t -> (unit,Term.Path.t) BatResult.t

  val get_conflicted_transitions : t -> RuleSet.t
  val conflict_free : t -> t
end

(*TODO Abstract to general tree automaton? *)

module Make = functor (Elt : HotInterfaces.ORDEREDPRINTABLE) -> 
      functor (Type : HotType.S) ->
struct

  module RankedAlphabet = HotRankedAlphabet.Make(Elt)(Type)
  module Term = HotTerm.Make(RankedAlphabet)

  type inner_state = string * Term.Path.t

  module State = HotExtBatSet.Make(struct type t = inner_state let compare = compare end)

  type state = State.t

  module States = HotExtBatSet.Make(struct type t = state let compare = State.compare end)

  type state_t =
    | SDrain
    | SStates of state list

  let string_of_state_internal (prefix, path) =
    if path = Term.Path.epsilon then
      Printf.sprintf "q_%s" prefix
    else
      Printf.sprintf "q_%s:%s" prefix @@ Term.Path.string_of path

  let string_of_state ss =
    let (fst,lst) =
      if State.cardinal ss = 1 then
        ("","")
      else
        ("{","}")
    in
    let io = BatIO.output_string () in
      State.print ~first:fst ~last:lst ~sep:(",")
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

    let get_states_dom_ran rs =
      let states_dom = ref States.empty in
      let states_ran = ref States.empty in
        iter (fun (q,_,_,qs) -> match qs with
                | SDrain -> states_dom := States.add q !states_dom
                | SStates(qs') ->
                    begin
                      states_dom := States.add q !states_dom;
                      states_ran := States.union !states_ran @@ States.of_list qs';
                    end
        ) rs;
        (!states_dom, !states_ran)

    let get_states rs =
      let (d,r) = get_states_dom_ran rs in
        States.union d r


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

  let get_rules carta = carta.rules

  let filter_rules carta p =
    {carta with rules = RuleSet.filter p carta.rules}

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

  let mk_state lst = match lst with
    | [] -> failwith "State must be inhabited by at least one function * path pair!"
    | _ -> State.of_list lst

  let mk_state_1 (prefix,path) = State.singleton (prefix,path)

  let mk_state_union ss = State.union_all ss

  let equal c1 c2 =
    RankedAlphabet.equal c1.s c2.s &&
    c1.qs = c2.qs &&
    RuleSet.equal c1.rules c2.rules &&
    c1.q = c2.q

  let disjoint_union qinitial cr1 cr2 =
    {
      s = RankedAlphabet.union cr1.s cr2.s;
      qs = States.union cr1.qs cr2.qs;
      rules = RuleSet.union cr1.rules cr2.rules;
      q = qinitial;
    }

  let update_q c =
    let states = RuleSet.get_states c.rules in
      { c with qs = States.add c.q states }

  let drain_close carta =
    let (dom,ran) = RuleSet.get_states_dom_ran carta.rules in
    let ran_qs = States.union carta.qs ran in
    let free_states = States.as_list @@ States.diff ran_qs dom in
    let star = Term.mkVar "*" in
    let rules = RuleSet.of_list @@ BatList.map (fun q -> (q,star,Term.Path.epsilon,mk_drain)) free_states in
      add_rules carta rules

  let accepts carta input =
    let open BatResult.Monad in
    let open Term in
    let open Term.Path in
    let open Term.Path.Infix in
    let return_err x = BatResult.Bad(x) in
    let rec accepts_inner path cfg : (unit, Term.Path.t) BatResult.t =
(*      Printf.printf "accepts_inner %s %s\n"
        (Term.Path.string_of ~epsilon:true path)
        (string_of_state cfg);*)
      (* Idea (for each cfg):
       p = path to term in delta
       path' = path with suffix p removed
       input' = input -. path
       check if input' ~ term
       *)
      let delta = RuleSet.as_list @@ get_transitions carta cfg in
      let check_transition ((q,t,p,qs) as rule) : (rule,Term.Path.t) BatResult.t =
        try
          (* remove_suffix might raise an exception! *)
          let path' = remove_suffix path p in
          let input' = input -. path' in
            match unify input' t with
              | BatResult.Bad(_) -> return_err path
              | BatResult.Ok(_) -> return rule (* TODO complete! *)
        with
          | HotTerm.Path_not_found_in_term -> return_err path
      in
      let delta_res = BatList.map check_transition delta in
      (* Exactly one result must be ok!
       if none is ok, then the input is not accepted.
       if >1   is ok, then we raise an exception!
       *)
      let (ok,bad) = BatList.partition BatResult.is_ok delta_res in
        match ok with
          | [] -> return_err path
          | Ok(q,t,p,qs) :: [] ->
              begin
                match qs with
                  | SDrain -> return () (* We accept anything below a drain *)
                  | SStates([]) -> return () (* No further states. We accept. *)
                  | SStates(qs) ->
                      (* This must be ctor, otherwise qs = drain! *)
                      match t -. p  with
                        | App(Ctor(re),_) ->
                            begin
                              let rec_call i q =
                                let path' = append path (Ele(re,i,Empty)) in
                                  accepts_inner path' q
                              in
                              let res = BatList.mapi rec_call qs in
                                match BatList.filter BatResult.is_bad res with
                                  | [] -> return ()
                                  | err :: _ -> err
                            end
                        | _ -> failwith "Ill-formed rule: t -. p != ctor and qs != drain"
              end
          | _ -> failwith "Non-deterministic transition rules!"
    in
      accepts_inner Term.Path.epsilon carta.q

  let get_conflicted_transitions ca =
    let open Term.Path in
    let open Term.Path.Infix in
    (* Check, which states/transitions are NOT conflict free *)
    let has_child_conflict (q,t,p,qs) =
      (* Fetch all child transitions and check if their contexts are unifiable with
       the current context. *)
      let check_unification i t' p' =
        match t -. p with
          | Term.App(Term.Ctor(c),_) ->
               BatResult.is_ok @@ Term.context_unification (t,append p (Ele(c,i,Empty))) (t',p')
          | _ -> failwith "Unexpected behavior, please report." (*BISECT-IGNORE*)
      in
      match qs with
        | SDrain -> false (* Drain states are not conflicted *)
        | SStates(qs') ->
            let f i q =
              let delta_q_i = RuleSet.as_list @@ get_transitions ca q in
                BatList.map (fun (_,ti,pi,_) -> check_unification i ti pi) delta_q_i
            in
            let result = BatList.concat @@ BatList.mapi f qs' in
              BatList.exists (fun x -> x = false) result
    in
      RuleSet.filter has_child_conflict ca.rules


  let conflict_free (ca : t) : t =
    (* Fetch conflicting transitions *)
    let trans = get_conflicted_transitions ca in
      (* Remove them, and all single predecessors *)
      failwith "Not yet implemented"

    (* Transformation *)
  module NBA : HotTreeAutomaton.S = struct

    module Rules : HotTreeAutomaton.RULES
      with type symbol = RankedAlphabet.elt = struct
        include HotExtBatSet.Make(
        struct 
          type t = string * RankedAlphabet.elt * string list
          let compare = compare
        end)

        type symbol = RankedAlphabet.elt
        type state = string

        let string_of_elt (q,a,qs) =
          Printf.sprintf "%s,%s -> %s"
            q
            (RankedAlphabet.string_of_elt a)
            (String.concat " " qs)

        let string_of rules =
          let io = BatIO.output_string () in
            print ~first:"{" ~last:"}" ~sep:","
              (fun out r -> BatIO.nwrite out (string_of_elt r)) io rules;
            BatIO.close_out io
      end

    include HotTreeAutomaton.Make(RankedAlphabet)(Rules)
  end

  let to_nbda (carta : t) : NBA.t =
    NBA.create [] [] [] ""
end
