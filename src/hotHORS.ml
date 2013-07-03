open HotTerm.SortTerm
open HotTerm.SortTerm.Infix

let logger = BatLogger.make_log "HotHORS"

let log_debug s lst =
    BatLogger.log
      logger
      BatLogger.DEBUG 
      (fun () -> (s,lst))

let log_info s lst =
    BatLogger.log
      logger
      BatLogger.INFO
      (fun () -> (s,lst))

let log_debug_in s lst = log_debug (s ^ " (in)") lst
let log_debug_out s lst = log_debug (s ^ " (out)") lst

module Term = HotTerm.SortTerm

exception Path_does_not_point_to_nonterminal_in_app
exception HORS_is_not_productive

module Rules = struct
  include HotExtBatSet.Make(struct type t = string * string list * HotTerm.SortTerm.t let
compare = compare end)

  exception No_suitable_rule_found

  let fetch rules symbol : elt =
    let rule' = filter (fun (f,_,_) -> f = symbol) rules in
      if is_empty rule' then
        raise No_suitable_rule_found
      else
        min_elt rule'
end

type ra = HotRankedAlphabet.StringRankedAlphabet.t
type raelt = HotRankedAlphabet.StringRankedAlphabet.elt

type t =
    {
      term : ra;
      nonterm : ra;
      rules : Rules.t;
      start : raelt;
    }

let create sigma n r s =
  (* TODO Check well-foundedness*)
  {
    term = sigma;
    nonterm = n;
    rules = r;
    start = s;
  }

let rule hors f = Rules.fetch hors.rules f

let is_safe hors = failwith "Not yet implemented"
                    
let is_well_formed hors = failwith "Not yet implemented"

let is_productive hors = true (*failwith "Not yet implemented"*)

let string_of hors = ""

let step hors term path =
  log_debug_in "step"
    [
      ("term",Term.string_of term);
      ("path",HotPath.string_of path);
    ];
  let res = 
  match term -. path with
    | App(Ctor(s,tp),ts) ->
        let (f,xs,newterm) = rule hors s in
          if List.length xs = List.length ts then
            begin
              let xsts = List.combine xs ts in
              let resterm = List.fold_left (fun ack (x,t) -> subst x t ack) newterm xsts in
                subst_path path resterm term
            end
          else
            begin
              log_debug "step partial app" [];
              (* We cannot reduce this term as it is only partially applied. *)
              term
            end
    | _ -> raise Path_does_not_point_to_nonterminal_in_app
  in
    log_debug_out "step"
      [
        ("res",Term.string_of res);
      ];
    res

let is_non_terminal s = BatChar.is_uppercase s.[0]

let has_terminal_head = function
  | Ctor(s,_) when not (is_non_terminal s) -> true
  | App(Ctor(s,_),_) when not (is_non_terminal s) -> true
  | _ -> false

let rec step_all hors = function
  | App(Ctor(s,tp),ts) when is_non_terminal s ->
      begin
        let (f,xs,t) = rule hors s in
        let xsts = List.combine xs ts in
          List.fold_left (fun ack (x,t) -> subst x t ack) t xsts
      end
  | App(Ctor(s,tp),ts) -> App(Ctor(s,tp),List.map (step_all hors) ts)
  | App(_ as t1, ts) -> App(t1,List.map (step_all hors) ts)
  | Ctor(_,_) as t -> t
  | Var(_) as t -> t
  | Bottom as t -> t

let step_all_until_terminal hors term =
  log_debug_in "step_all_until_terminal"
    [
      ("hors","N/A");
      ("term",Term.string_of term);
    ];
  if not (is_productive hors) then
    raise HORS_is_not_productive
  else
    let rec inner = function
      | App(Ctor(s,tp),ts) as term when is_non_terminal s ->
          begin
            let rec one_reduction term =
              let res = step hors term HotPath.epsilon in
                if has_terminal_head res then
                  res
                else
                  one_reduction res
            in
              one_reduction term
          end
      | App(Ctor(s,tp),ts) -> App(Ctor(s,tp),List.map inner ts)
      | App(App(_,_) as t,ts) -> App(inner t, ts) (* TODO IO/OI *)
      | App(_,_) as t -> t
      | Ctor(_,_) as t -> t
      | Var(x) as t -> t
      | Bottom as t -> t
    in
    let res = inner term in
      log_debug_out "step_all_until_terminal"
        [
          ("result", Term.string_of res);
        ];
      res


module TermSet = struct
  include HotExtBatSet.Make(struct type t = HotTerm.SortTerm.t let compare = compare end)

  let default_print ?(sort=false) set =
    let io = BatIO.output_string () in
      print ~first:"{" ~last:"}" ~sep:(",") (fun out t -> BatIO.nwrite out (HotTerm.SortTerm.string_of ~sort:sort t)) io set;
      BatIO.close_out io
end


let rec cut_after_n n term =
  if n = 0 then
    Bottom
  else
    match term with
      | App(Ctor(s,tp),ts) -> App(Ctor(s,tp),List.map (cut_after_n (n-1)) ts)
      | App(_,_) -> term
      | Ctor(_,_) -> term
      | Var(_) -> term
      | Bottom -> term

let rec cut_nonterminals t =
  log_debug_in "cut_nonterminals" [("t", Term.string_of t)];
  let res = match t with
    | App(Ctor(s,_),_) when is_non_terminal s -> Bottom
    | App(Ctor(s,tp),ts) -> App(Ctor(s,tp),List.map cut_nonterminals ts)
    | App(t,ts) ->
        let t_without = cut_nonterminals t in
          if t_without = Bottom then
            Bottom
          else
            App(t_without,List.map cut_nonterminals ts)
    | _ as t -> t
  in
    log_debug_out "cut_nonterminals" [("res", Term.string_of res)];
    res

let rec cut_bottom_leaves t = 
  log_debug_in "cut_bottom_leaves" [("t", Term.string_of t)];
  let res = match t with
    | App(t,ts) when List.for_all (fun x -> x = Bottom) ts -> App(t,[])
    | App(Ctor(s,tp),ts) -> App(Ctor(s,tp),List.map cut_bottom_leaves ts)
    | _ as t -> t
  in
    log_debug_out "cut_bottom_leaves" [("res", Term.string_of res)];
    res

let get_contexts n term =
  log_debug_in "get_contexts"
    [
      ("n",string_of_int n);
      ("term",Term.string_of term);
    ];
  let term' = cut_nonterminals term in
  let rec ctxt_internal n t = match t with
    | App(App(_,_),ts)
    | App(Ctor(_,_),ts) ->
        let res = cut_after_n n t in
          TermSet.add res (TermSet.union_all (List.map (ctxt_internal n) ts))
    | Bottom -> TermSet.empty
    | Ctor(s,tp) as term -> TermSet.singleton term
    | App(t,_) -> failwith ("Non-ctor applications are not allowed: " ^
                            (Term.string_of t))
    | Var(_) -> failwith "Variables are not allowed"
  in
  let res = ctxt_internal n term' in
    log_debug_out "get_contexts" [("result", TermSet.default_print res)];
    res

let contexts (hors : t) (depth : int) : TermSet.t =
  let sname,stp = hors.start in
  let rec fp_run (ctxts : TermSet.t) (s : HotTerm.SortTerm.t) =
    let str_ctxts = TermSet.default_print ctxts in
    let str_term = Term.string_of s in
    log_debug_in "fp_run" [("ctxts", str_ctxts);("s",str_term)];
    let t = step_all_until_terminal hors s in
    let new_ctxts = get_contexts depth t in
    let new_ctxts' = TermSet.filter (fun t -> Term.depth t = depth + 1) new_ctxts in
    let initial_ctxt = cut_bottom_leaves (cut_after_n 1 t) in
    let stripped_ctxts = TermSet.map cut_bottom_leaves new_ctxts' in
    let ctxts' = TermSet.union_all [ctxts;stripped_ctxts;TermSet.singleton initial_ctxt] in
      log_info "ctxts"
        [
          ("before", (TermSet.default_print ctxts));
          ("after", (TermSet.default_print ctxts'));
        ];
      if TermSet.equal ctxts ctxts' then
        begin
          log_info "fp_run FIXPOINT" [("ctxts", (TermSet.default_print ctxts'))];
          ctxts
        end
      else
        fp_run ctxts' t
  in
    fp_run TermSet.empty (App(Ctor(sname,stp),[]))

