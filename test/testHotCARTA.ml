(** Test module for HotTerm. *)

open Batteries

open Test
open HotSimple.SimpleCARTA
open HotType.Sort.Infix

module T = Term
module P = T.Path
module RA = RankedAlphabet

open T
open HotSimple.SimpleCARTA

let zero = ("zero", o)
let nil = ("nil", o)
let foo = ("foo", o)
let succ = ("succ", o ^=> o)
let pair = ("pair", o ^=> o ^=> o)
let unary = ("unary", o ^=> o)

let mkZero = mkCtor zero
let mkNil = mkCtor nil
let mkFoo = mkCtor foo
let mkSucc a = mkApp (mkCtor succ) [a]
let mkPair a b = mkApp (mkCtor pair) [a;b]
let mkUnary a = mkApp (mkCtor unary) [a]

let q0 = State.singleton ("0",Term.Path.Empty)
let q1 = State.singleton ("1",Term.Path.Empty)

(* accepts *)

(* A CARTA that accepts one element only. *)
let carta_zero =
  let sigma = RA.singleton zero in
  let q = mk_state_1 ("f",P.epsilon) in
  let states = States.singleton q in
  let r = (q, mkCtor zero, P.epsilon, mk_states []) in
  let rules = RuleSet.singleton r in
    create sigma states rules q

(** A CARTA that accepts numbers, i.e. zero/succ lists *)
let carta_number = 
  let sigma = RA.of_list [zero;succ] in
  let q = mk_state_1 ("f",P.epsilon) in
  let states = States.singleton q in
  let r1 = (q, mkCtor zero, P.epsilon, mk_states []) in
  let r2 = (q, mkApp (mkCtor succ) [mkVar "x"], P.epsilon, mk_states [q]) in
  let rules = RuleSet.of_list [r1;r2] in
    create sigma states rules q

(** A CARTA that accepts numbers, i.e. zero/succ lists *)
let carta_succ_drain =
  let sigma = RA.of_list [succ] in
  let q = mk_state_1 ("f",P.epsilon) in
  let states = States.singleton q in
  let r = (q, mkSucc (mkVar "*"), P.epsilon, mk_drain) in
  let rules = RuleSet.singleton r in
    create sigma states rules q

(** A CARTA that accepts pairs where each element can be either a pair
  or nil (1st) or a number (snd) *)
let carta_pair =
  let sigma = RA.of_list [succ] in
  let q = mk_state_1 ("f",P.epsilon) in
  let q_fst = mk_state_1 ("fst",P.epsilon) in
  let q_snd = mk_state_1 ("snd",P.epsilon) in
  let q_num = mk_state_1 ("num",P.epsilon) in
  let states = States.of_list [q;q_fst;q_snd;q_num] in
  let r = (q, mkPair (mkVar "a") (mkVar "b"), P.epsilon, mk_states [q_fst;q_snd]) in
  let r_fst_1 = (q_fst, mkPair (mkVar "a") (mkVar "b"), P.epsilon, mk_states [q_fst;q_snd]) in
  let r_fst_2 = (q_fst, mkNil, P.epsilon, mk_states []) in
  let r_snd_1 = (q_snd, mkPair (mkVar "a") (mkVar "b"), P.epsilon, mk_states [q_fst;q_snd]) in
  let r_snd_2 = (q_snd, mkZero, P.epsilon, mk_states []) in
  let r_snd_3 = (q_snd, mkSucc (mkVar "x"), P.epsilon, mk_states [q_num]) in
  let r_num_1 = (q_num, mkZero, P.epsilon, mk_states []) in
  let r_num_2 = (q_num, mkSucc (mkVar "x"), P.epsilon, mk_states [q_num]) in
  let rules = RuleSet.of_list
                [r;r_fst_1;r_fst_2;r_snd_1;r_snd_2;r_snd_3;r_num_1;r_num_2] in
    create sigma states rules q

(* A CARTA that accepts succ(zero) but requires context. *)
let carta_ctxt_succ_zero =
  let open Term.Path in
  let sigma = RA.singleton zero in
  let q = mk_state_1 ("f",P.epsilon) in
  let q2 = mk_state_1 ("g",P.epsilon) in
  let states = States.of_list [q;q2] in
  let r1 = (q, mkSucc (mkVar "x"), P.epsilon, mk_states [q2]) in
  let r2 = (q2, mkSucc (mkZero), Ele(succ,0,Empty), mk_states []) in
  let rules = RuleSet.of_list [r1;r2] in
    create sigma states rules q

(** A CARTA that depending on the context accepts different stuff *)
let carta_ctxt_2 =
  let open Term.Path in
  let sigma = RA.of_list [pair] in
  let q = mk_state_1 ("f",P.epsilon) in
  let q_fst = mk_state_1 ("fst",P.epsilon) in
  let q_snd = mk_state_1 ("snd",P.epsilon) in
  let states = States.of_list [q;q_fst;q_snd] in
  let r = (q, mkPair (mkVar "x") (mkVar "y"), P.epsilon, mk_states [q_fst;q_snd]) in
  let r_fst_1 = (q_fst, mkPair (mkZero) (mkVar "y"), Ele(pair,0,Empty), mk_states []) in
  let r_fst_2 = (q_fst, mkPair (mkNil)  (mkVar "y"), Ele(pair,0,Empty), mk_states []) in
  let r_snd_1 = (q_snd, mkPair (mkZero) (mkVar "*"), Ele(pair,1,Empty), mk_drain) in
  let r_snd_2 = (q_snd, mkPair (mkNil) (mkNil), Ele(pair,1,Empty), mk_states []) in
  let rules = RuleSet.of_list [r;r_fst_1;r_fst_2;r_snd_1;r_snd_2] in
    create sigma states rules q

(** Helper function for acceptance tests *)
let test_accept carta input =
  let out = accepts carta input in
  let txt = Printf.sprintf "CARTA should accept %s but reported error in path %s"
              (Term.string_of input)
              (match out with
                 | Bad(p) -> Path.string_of p
                 | Ok(()) -> "")
  in
    assert_bool txt @@ BatResult.is_ok out

let test_accept_bad carta input =
  let out = accepts carta input in
  let txt = Printf.sprintf "CARTA should NOT accept %s but reported ok"
              (Term.string_of input)
  in
    assert_bool txt @@ not @@ BatResult.is_ok out


(* Simple acceptance checks on CARTAs w/o context! *)

let test_accept_zero_ok () =
  let term = mkZero in
    test_accept carta_zero term

let test_accept_nil_bad () =
  let term = mkNil in
    test_accept_bad carta_zero term

let test_accept_zero_num_ok () =
  let term = mkZero in
    test_accept carta_number term

let test_accept_succ_zero_num_ok () =
  let term = mkSucc mkZero in
    test_accept carta_number term

let test_accept_succ_zero_succ_drain_ok () =
  let term = mkSucc mkZero in
    test_accept carta_succ_drain term

let test_accept_nested_pair_carta_pair_1_ok () =
  let term = mkPair mkNil mkZero in
    test_accept carta_pair term

let test_accept_nested_pair_carta_pair_2_ok () =
  let term = mkPair mkNil (mkSucc mkZero) in
    test_accept carta_pair term

let test_accept_nested_pair_carta_pair_3_ok () =
  let term = mkPair (mkPair mkNil mkZero) (mkPair mkNil (mkSucc mkZero)) in
    test_accept carta_pair term

let test_accept_nested_pair_carta_pair_1_bad () =
  let term = mkPair (mkPair mkZero mkNil) (mkPair mkNil (mkSucc mkZero)) in
    test_accept_bad carta_pair term

let test_accept_nested_pair_carta_pair_2_bad () =
  let term = mkPair (mkPair mkNil mkZero)
               (mkPair mkNil (mkSucc (mkSucc (mkSucc (mkSucc mkNil))))) in
    test_accept_bad carta_pair term

(* Acceptance checks that require contexts *)

let test_accept_ctxt_succ_zero_ok () =
  let term = mkSucc mkZero in
    test_accept carta_ctxt_succ_zero term

let test_accept_ctxt_succ_nil_bad () =
  let term = mkSucc mkNil in
    test_accept_bad carta_ctxt_succ_zero term

let test_accept_ctxt_1_ok () =
  let term = mkPair mkNil mkNil in
    test_accept carta_ctxt_2 term

let test_accept_ctxt_2_ok () =
  let term = mkPair mkZero mkNil in
    test_accept carta_ctxt_2 term

let test_accept_ctxt_3_ok () =
  let term = mkPair mkZero mkZero in
    test_accept carta_ctxt_2 term

let test_accept_ctxt_4_ok () =
  let term = mkPair mkZero mkFoo in
    test_accept carta_ctxt_2 term

let test_accept_ctxt_5_ok () =
  let term = mkPair mkNil mkNil in
    test_accept carta_ctxt_2 term

let test_accept_ctxt_6_bad () =
  let term = mkPair mkNil mkZero in
    test_accept_bad carta_ctxt_2 term

let test_accept_ctxt_7_bad () =
  let term = mkPair mkFoo mkZero in
    test_accept_bad carta_ctxt_2 term

(* get_conflicting_transitions *)

let mk_carta_delta delta =
  create RankedAlphabet.empty States.empty delta
    State.empty

let carta_no_conflict =
  let open Term.Path in
  let delta = RuleSet.of_list
                [
                  (q0,mkSucc @@ mkVar "x", Empty, SStates([q1]));
                  (q1,mkSucc mkZero, Ele(succ,0,Empty), SStates([]))
                ]
  in
    mk_carta_delta delta

let test_get_conflicting_transitions_none () =
  let open Term.Path in
  let res = get_conflicted_transitions carta_no_conflict in
    assert_bool
      "Set of conflicting transitions should be empty"
      (RuleSet.is_empty res)

let test_get_conflicting_transitions_none_smaller () =
  let open Term.Path in
  let delta = RuleSet.of_list
                [
                  (q0,mkSucc @@ mkVar "x", Empty, SStates([q1]));
                  (q1,mkZero, Empty, SStates([]))
                ]
  in
  let carta = mk_carta_delta delta in
  let res = get_conflicted_transitions carta in
    assert_bool
      "Set of conflicting transitions should be empty"
      (RuleSet.is_empty res)

let test_get_conflicting_transitions_one () =
  let open Term.Path in
  let delta = RuleSet.of_list
                [
                  (q0,mkSucc @@ mkVar "x", Empty, SStates([q1]));
                  (q1,mkPair (mkVar "y") @@ mkZero, Ele(pair,0,Empty), SStates([]))
                ]
  in
  let carta = mk_carta_delta delta in
  let res = get_conflicted_transitions carta in
    assert_bool
      "Set of conflicting transitions must not be empty"
      (not @@ RuleSet.is_empty res)

(* conflict-free *)

let test_conflict_free_no_conflict () =
  let open Term.Path in
  let open HotSimple.SimpleCARTA in
  let res = conflict_free carta_no_conflict in
    assert_equal ~cmp:equal ~printer:string_of carta_no_conflict res

let test_conflict_free_partial_conflict () =
  let open Term.Path in
  let open HotSimple.SimpleCARTA in
  let delta = RuleSet.of_list
                [
                  (q1,mkUnary mkZero, Ele(foo,0,Empty), SStates([]));
                ]
  in
  let carta = add_rules carta_no_conflict delta in
  let res = conflict_free carta in
    assert_equal ~cmp:equal ~printer:string_of carta_no_conflict res


let init_tests () =
  [
   ("accept zero ok", test_accept_zero_ok);
   ("accept nil bad", test_accept_nil_bad);
   ("accept zero num ok", test_accept_zero_num_ok);
   ("accept succ_zero num ok", test_accept_succ_zero_num_ok);
   ("accept succ_zero succ_drain ok", test_accept_succ_zero_succ_drain_ok);
   ("accept nested_pair pair ok 1", test_accept_nested_pair_carta_pair_1_ok);
   ("accept nested_pair pair ok 2", test_accept_nested_pair_carta_pair_2_ok);
   ("accept nested_pair pair ok 3", test_accept_nested_pair_carta_pair_3_ok);
   ("accept nested_pair pair bad 1", test_accept_nested_pair_carta_pair_1_bad);
   ("accept nested_pair pair bad 2", test_accept_nested_pair_carta_pair_2_bad);
   ("accept simple context succ zero ok", test_accept_ctxt_succ_zero_ok);
   ("accept simple context succ nil ok", test_accept_ctxt_succ_nil_bad);
   ("accept context-dependant drain 1 ok", test_accept_ctxt_1_ok);
   ("accept context-dependant drain 2 ok", test_accept_ctxt_2_ok);
   ("accept context-dependant drain 3 ok", test_accept_ctxt_3_ok);
   ("accept context-dependant drain 4 ok", test_accept_ctxt_4_ok);
   ("accept context-dependant drain 5 ok", test_accept_ctxt_5_ok);
   ("accept context-dependant drain 6 bad", test_accept_ctxt_6_bad);
   ("accept context-dependant drain 7 bad", test_accept_ctxt_7_bad);
   ("get_conflicting_transitions none", test_get_conflicting_transitions_none);
   ("get_conflicting_transitions none smaller", test_get_conflicting_transitions_none_smaller);
   ("get_conflicting_transitions one", test_get_conflicting_transitions_one);
   ("conflict_free no conflict", test_conflict_free_no_conflict);
   ("conflict_free partial conflict", test_conflict_free_partial_conflict);
  ]

let _ = install_tests_new "HotCARTA" init_tests
