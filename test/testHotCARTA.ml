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

(* accepts *)

(* A CARTA that accepts one element only. *)
let carta_zero =
  let sigma = RA.singleton zero in
  let q = mk_single_state ("f",P.epsilon) in
  let states = States.singleton q in
  let r = (q, mkCtor zero, P.epsilon, mk_drain) in
  let rules = RuleSet.singleton r in
    create sigma states rules q

let test_accept_zero_ok () =
  let term = mkCtor zero in
    assert_bool "Zero CARTA should accept term" @@ BatResult.is_ok @@ accepts carta_zero term

let test_accept_nil_bad () =
  let term = mkCtor nil in
    assert_bool "Zero CARTA must not accept term" @@ BatResult.is_bad @@ accepts carta_zero term

let init_tests () =
  [
   ("accept zero ok", test_accept_zero_ok);
   ("accept nil bad", test_accept_nil_bad);
  ]

let _ = install_tests_new "accept" init_tests
