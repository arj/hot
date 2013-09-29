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
let succ = ("succ", o ^=> o)
let pair = ("pair", o ^=> o ^=> o)

let mkZero = mkCtor zero
let mkNil = mkCtor nil
let mkSucc a = mkApp (mkCtor succ) [a]
let mkPair a b = mkApp (mkCtor pair) [a;b]

(* accepts *)

(* A CARTA that accepts one element only. *)
let carta_zero =
  let sigma = RA.singleton zero in
  let q = mk_single_state ("f",P.epsilon) in
  let states = States.singleton q in
  let r = (q, mkCtor zero, P.epsilon, mk_states []) in
  let rules = RuleSet.singleton r in
    create sigma states rules q

(** A CARTA that accepts numbers, i.e. zero/succ lists *)
let carta_number = 
  let sigma = RA.of_list [zero;succ] in
  let q = mk_single_state ("f",P.epsilon) in
  let states = States.singleton q in
  let r1 = (q, mkCtor zero, P.epsilon, mk_states []) in
  let r2 = (q, mkApp (mkCtor succ) [mkVar "x"], P.epsilon, mk_states [q]) in
  let rules = RuleSet.of_list [r1;r2] in
    create sigma states rules q

(** A CARTA that accepts numbers, i.e. zero/succ lists *)
let carta_succ_drain =
  let sigma = RA.of_list [succ] in
  let q = mk_single_state ("f",P.epsilon) in
  let states = States.singleton q in
  let r = (q, mkSucc (mkVar "*"), P.epsilon, mk_drain) in
  let rules = RuleSet.singleton r in
    create sigma states rules q

(** A CARTA that accepts pairs where each element can be either a pair
  or nil (1st) or a number (snd) *)
let carta_pair =
  let sigma = RA.of_list [succ] in
  let q = mk_single_state ("f",P.epsilon) in
  let q_fst = mk_single_state ("fst",P.epsilon) in
  let q_snd = mk_single_state ("snd",P.epsilon) in
  let q_num = mk_single_state ("num",P.epsilon) in
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
  ]

let _ = install_tests_new "HotCARTA" init_tests
