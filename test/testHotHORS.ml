(** Test module for HotHORS. *)

(* For debug output, set log level in testMain.ml *)

open Test
module T = HotTerm.SortTerm
open HotTerm.SortTerm.Infix
module RA = HotRankedAlphabet.StringRankedAlphabet
module Sort = HotTypes.Sort
open HotHORS
open HotTypes.Sort.Infix

let r1 = ("A", ["x1";"x2"],
          T.App(T.Ctor("b", o ^=> o ^=> o), [T.Var("x1");T.Var("x2")])) 
let r2 = ("F", [], T.App(T.Ctor("G",o),[])) 
let r3 = ("G", [], T.App(T.Ctor("A",o ^=> o ^=> o),[T.Bottom;T.Bottom]))
let r4 = ("B", [], T.Bottom)
let rules = Rules.add r4 (Rules.add r1 (Rules.add r2 (Rules.singleton r3)))

let hors1 =
  let ra = RA.empty in
  let s = ("S", o) in
    create ra ra rules s

module Ctxt = struct
  let r1 = ("A", ["x1";"x2"],
            T.App(T.Ctor("b", o ^=> o ^=> o), [T.Var("x1");T.Var("x2")])) 
  let r2 = ("F", [], T.App(T.Ctor("G",o),[])) 
  let r3 = ("G", [], T.App(T.Ctor("A",o ^=> o ^=> o),[T.Bottom;T.Bottom]))
  let rules = Rules.of_list [r1;r2;r3]

  let hors_a_bb =
    let r1 = ("S", [], T.App(T.Ctor("F",o ^=> o), [T.App(T.Ctor("b", o ^=> o), [])])) in
    let r2 = ("F", ["x"], T.App(T.Ctor("a", o ^=> o ^=> o),
                                [T.App(T.Ctor("F",o ^=> o), [T.App(T.Ctor("b", o
                                                                          ^=> o), [])]);
                                 T.App(T.Ctor("F",o ^=> o), [T.App(T.Ctor("b", o
                                                                          ^=> o), [])])]
    )) in
    let rules = Rules.of_list [r1;r2] in
    let ra = RA.empty in
    let s = ("S", o) in
      create ra ra rules s

  let hors_a =
    let rules = Rules.singleton
                  ("S",[], T.App(T.Ctor("a",o ^=> o),[T.App(T.Ctor("S",o),[])])) in
      create RA.empty RA.empty rules ("S", o)

  let rules_abcd =
    let nF x1 x2 = T.App(T.Ctor("F", o ^=> o ^=> o),[x1;x2]) in
    let a x1 x2 =  T.App(T.Ctor("a", o ^=> o ^=> o),[x1;x2]) in
    let b = T.App(T.Ctor("b", o ^=> o), []) in
    let d = T.App(T.Ctor("d", o ^=> o), []) in
    let c = T.App(T.Ctor("c", o), []) in
    let r1 = ("S", [], nF b d) in
    let r2 = ("F", ["x1";"x2"],
              a c (T.App(T.Var("x1"), [nF (T.Var("x2")) (T.Var("x1"))]))) in
      Rules.of_list [r1;r2]

  let hors_abcd =
      create RA.empty RA.empty rules_abcd ("S", o)

  let hors_1 =
    let b = T.App(T.Ctor("b", o),[]) in
    let c = T.App(T.Ctor("c", o),[]) in
    let f x = T.App(T.Ctor("F", o ^=> o ^=> o),[x]) in
    let a x1 x2 = T.App(T.Ctor("a", o ^=> o ^=> o),[x1;x2]) in
    let r1 = ("S", [], T.App(T.Ctor("F", o ^=> o ^=> o),
                             [T.App(T.Ctor("G", o ^=> o ^=> o),[b]);c])) in
    let r2 = ("F", ["phi";"x"], a (T.App(T.Var("phi"), [f (T.Var("phi"))]))
                                  (T.Var("x"))) in
    let r3 = ("G", ["y";"psi"], T.App(T.Var("psi"), [T.Var("y")])) in
    let rules = Rules.of_list [r1;r2;r3] in
      create RA.empty RA.empty rules ("S", o)

  let hors_2 =
    let r1 = ("S", [], T.App(T.Ctor("H", o ^=> o),
                             [T.App(T.Ctor("a",o), [])])) in
    let r2 = ("H", ["z"], T.App(T.Ctor("F", (o ^=> o) ^=> o),
                                [T.App(T.Ctor("g", o ^=> o ^=> o),
                                       [T.Var("z")])])) in
    let r3 = ("F", ["phi"], T.App(T.Var("phi"), [T.App(T.Var("phi"),
                                                       [T.App(T.Ctor("F",(o ^=> o) ^=> o),
                                                              [T.App(T.Ctor("h", o ^=> o),[])])])])) in
      create RA.empty RA.empty (Rules.of_list [r1;r2;r3]) ("S", o)

  let hors_saddle_point =
    let f x = T.App(T.Ctor("F", (o ^=> o) ^=> o), [x]) in
    let b = T.App(T.Ctor("b", o), []) in
    let r1 = ("S", [], f (f (f b))) in
    let r2 = ("F", ["phi"], T.App(T.Ctor("a", o ^=> o), [T.Var("phi")])) in
      create RA.empty RA.empty (Rules.of_list [r1;r2]) ("S", o)
end


(* rule *)

let test_rule_fetch_present () =
  let exp = r3 in
  let out = Rules.fetch rules "G" in
    assert_equal exp out

let test_rule_fetch_not_present () =
  let f () = Rules.fetch rules "RuleNotPresent" in
    assert_raises (Rules.No_suitable_rule_found) f

(* step *)

let test_step_non_terminal_empty_path () =
  let exp = T.App(T.Ctor("G", o), []) in
  let out = step hors1 (T.App(T.Ctor("F", o), [])) HotPath.epsilon in
    assert_equal ~printer:(T.string_of) exp out

let test_step_direct_ctor () =
  let exp = T.Bottom in
  let out = step hors1 (T.App(T.Ctor("B", o),[])) HotPath.epsilon in
    assert_equal ~printer:(T.string_of) exp out

let test_step_app_ctor () =
  let exp = T.Bottom in
  let out = step hors1 (T.App(T.Ctor("B", o),[])) HotPath.epsilon in
    assert_equal ~printer:(T.string_of) exp out
 
(* TODO S -> F G; F x -> x b; G x -> x *)

(* TODO Missing *)

(* step_all *)

let test_step_all_var () =
  let exp = T.Var("x") in
  let out = step_all hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_bottom () =
  let exp = T.Bottom in
  let out = step_all hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_terminal () =
  let exp = T.App(T.Ctor("a", o ^=> o), [T.Bottom]) in
  let out = step_all hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_non_terminal () =
  let exp = T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom]) in
  let out = step_all hors1 (T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_non_terminal_no_rule () =
  let exp = T.App(T.Ctor("QNoRule", o), []) in 
  let f () = step_all hors1 exp in
    assert_raises (Rules.No_suitable_rule_found) f

let test_step_all_terminal_non_terminal () =
  let exp = T.App(T.Ctor("c", o ^=> o), [T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom])]) in
  let out = step_all hors1
              (T.App(T.Ctor("c",o ^=> o), [T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_non_terminal_nested () =
  let exp = T.App(T.Ctor("b", o ^=> o ^=> o), [(T.App(T.Ctor("A", o ^=> o ^=> o),
                                                [T.Bottom; T.Bottom])); T.Bottom]) in
  let out = step_all hors1
              (T.App(T.Ctor("A", o ^=> o ^=> o), [(T.App(T.Ctor("A", o ^=> o ^=> o),
                                                   [T.Bottom; T.Bottom])); T.Bottom])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_terminal_multiple_non_terminal () =
  let exp = T.App(T.Ctor("c", o ^=> o), [T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom]);
                                  T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom])]) in
  let out = step_all hors1
              (T.App(T.Ctor("c",o ^=> o), [T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom]);
                                    T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])])) in
    assert_equal ~printer:(T.string_of) exp out

(* step_all_until_terminal *)

let test_step_all_until_terminal_var () =
  let exp = T.Var("x") in
  let out = step_all_until_terminal hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_bottom () =
  let exp = T.Bottom in
  let out = step_all_until_terminal hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_terminal () =
  let exp = T.App(T.Ctor("a", o ^=> o), [T.Bottom]) in
  let out = step_all_until_terminal hors1 exp in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_non_terminal () =
  let exp = T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom]) in
  let out = step_all_until_terminal hors1 (T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_non_terminal_no_rule () =
  let exp = T.App(T.Ctor("QNoRule", o), []) in 
  let f () = step_all_until_terminal hors1 exp in
    assert_raises (Rules.No_suitable_rule_found) f

let test_step_all_until_terminal_terminal_non_terminal () =
  let exp = T.App(T.Ctor("c", o ^=> o), [T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom])]) in
  let out = step_all_until_terminal hors1
              (T.App(T.Ctor("c",o ^=> o), [T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_non_terminal_nested () =
  let exp = T.App(T.Ctor("b", o ^=> o ^=> o), [(T.App(T.Ctor("A", o ^=> o ^=> o),
                                                [T.Bottom; T.Bottom])); T.Bottom]) in
  let out = step_all_until_terminal hors1
              (T.App(T.Ctor("A", o ^=> o ^=> o), [(T.App(T.Ctor("A", o ^=> o ^=> o),
                                                   [T.Bottom; T.Bottom])); T.Bottom])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_terminal_multiple_non_terminal () =
  let exp = T.App(T.Ctor("c", o ^=> o), [T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom]);
                                  T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom])]) in
  let out = step_all_until_terminal hors1
              (T.App(T.Ctor("c",o ^=> o), [T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom]);
                                    T.App(T.Ctor("A", o ^=> o ^=> o), [T.Bottom; T.Bottom])])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_terminal_multiple_non_terminal_steps () =
  let exp = T.App(T.Ctor("c", o ^=> o), [T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom]);
                                  T.App(T.Ctor("b", o ^=> o ^=> o), [T.Bottom; T.Bottom])]) in
  let out = step_all_until_terminal hors1
              (T.App(T.Ctor("c",o ^=> o), [T.App(T.Ctor("F", o), []);
                                    T.App(T.Ctor("F", o), [])])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_abcd () =
  let nF x1 x2 = T.App(T.Ctor("F", o ^=> o ^=> o),[x1;x2]) in
  let a x1 x2 =  T.App(T.Ctor("a", o ^=> o ^=> o),[x1;x2]) in
  let b = T.App(T.Ctor("b", o ^=> o), []) in
  let d = T.App(T.Ctor("d", o ^=> o), []) in
  let c = T.App(T.Ctor("c", o), []) in
  let exp = a c (T.App(T.Ctor("b", o ^=> o), [nF d b])) in
  let out = step_all_until_terminal Ctxt.hors_abcd (T.App(T.Ctor("S",o),[])) in
    assert_equal ~printer:(T.string_of) exp out

let test_step_all_until_terminal_complex_1 () =
  let exp = T.Bottom in
  let out = step_all_until_terminal Ctxt.hors_1 (T.App(T.Ctor("S",o),[])) in
  let out = step_all_until_terminal Ctxt.hors_1 out in
  let out = step_all_until_terminal Ctxt.hors_1 out in
    assert_equal ~printer:(T.string_of) exp out

(* Contexts *)
let test_context_a_depth_1 () =
  let exp = TermSet.singleton (T.App(T.Ctor("a", o ^=> o),[])) in
  let out = contexts Ctxt.hors_a 1 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_a_depth_2 () =
  let exp = TermSet.of_list 
              [T.App(T.Ctor("a", o ^=> o),[]);
               T.App(T.Ctor("a", o ^=> o),
                     [T.App(T.Ctor("a", o ^=> o),[])]);
              ] in
  let out = contexts Ctxt.hors_a 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_a_b_depth_2 () =
  let exp = TermSet.of_list 
              [T.App(T.Ctor("a", o ^=> o ^=> o),[]);
               T.App(T.Ctor("a", o ^=> o ^=> o),
                     [T.App(T.Ctor("a", o ^=> o ^=> o),[]);
                     T.App(T.Ctor("a", o ^=> o ^=> o),[])]);
              ] in
  let out = contexts Ctxt.hors_a_bb 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_abcd_depth_2 () =
  let a = T.App(T.Ctor("a", o ^=> o ^=> o),[]) in
  let qa x1 x2 = T.eta_reduce (T.App(a, [x1;x2])) in
  let b = T.App(T.Ctor("b", o ^=> o),[]) in
  let qb x = T.eta_reduce (T.App(b,[x])) in
  let c = T.App(T.Ctor("c",o),[]) in
  let d = T.App(T.Ctor("d",o ^=> o),[]) in
  let qd x = T.eta_reduce (T.App(d,[x])) in
  let exp = TermSet.of_list
              [
                a;
                qa c b;
                qa c d;
                qb a;
                qd a;
              ]
  in
  let out = contexts Ctxt.hors_abcd 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_hors_1 () =
  let exp = TermSet.empty in
  let out = contexts Ctxt.hors_1 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_hors_2 () =
  let exp = TermSet.empty in
  let out = contexts Ctxt.hors_2 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let test_context_hors_saddle_point () =
  let a = T.App(T.Ctor("a", o ^=> o),[]) in
  let qa x = T.eta_reduce (T.App(a, [x])) in
  let b = T.App(T.Ctor("b", o),[]) in
  let exp = TermSet.of_list
              [
                a;
                qa a;
                qa b;
              ]
  in
  let out = contexts Ctxt.hors_saddle_point 2 in
    assert_equal ~cmp:TermSet.equal ~printer:(TermSet.default_print) exp out

let init_tests () =
  [
    ("step non-terminal empty path", test_step_non_terminal_empty_path);
    ("step direct ctor", test_step_direct_ctor);
    ("step app ctor", test_step_app_ctor);
    ("rules fetch present", test_rule_fetch_present);
    ("rules fetch not present", test_rule_fetch_not_present);
    ("step_all variable", test_step_all_var);
    ("step_all bottom", test_step_all_bottom);
    ("step_all terminal", test_step_all_terminal);
    ("step_all non-terminal", test_step_all_non_terminal);
    ("step_all terminal non-terminal", test_step_all_terminal_non_terminal);
    ("step_all non-terminal no rule", test_step_all_non_terminal_no_rule);
    ("step_all non-terminal nested", test_step_all_non_terminal_nested);
    ("step_all terminal multiple non-terminal", test_step_all_terminal_multiple_non_terminal);
    ("step_all_until_terminal variable",test_step_all_until_terminal_var);
    ("step_all_until_terminal bottom",test_step_all_until_terminal_bottom);
    ("step_all_until_terminal terminal",test_step_all_until_terminal_terminal);
    ("step_all_until_terminal non-terminal",test_step_all_until_terminal_non_terminal);
    ("step_all_until_terminal terminal non-terminal",test_step_all_until_terminal_terminal_non_terminal);
    ("step_all_until_terminal non-terminal no rule",test_step_all_until_terminal_non_terminal_no_rule);
    ("step_all_until_terminal non-terminal nested",test_step_all_until_terminal_non_terminal_nested);
    ("step_all_until_terminal terminal multiple non-terminal",test_step_all_until_terminal_terminal_multiple_non_terminal);
    ("step_all_until_terminal terminal multiple non-terminal steps",test_step_all_until_terminal_terminal_multiple_non_terminal_steps);
    ("step_all_until_terminal abcd", test_step_all_until_terminal_abcd);
    (*("step_all_until_terminal complex 1", test_step_all_until_terminal_complex_1);*) (*TODO HORS not well founded? *)
    ("context a ->  ... -> a depth=1", test_context_a_depth_1);
    ("context a ->  ... -> a depth=2", test_context_a_depth_2);
    ("context a -> b ... depth=2", test_context_a_b_depth_2);
    ("context complex depth=2", test_context_abcd_depth_2);
(*    ("context complex 2 depth=2", test_context_hors_1);*)
    ("context complex 3 depth=2", test_context_hors_2);
    ("context complex saddle_point depth=2", test_context_hors_saddle_point);
  ]

let _ = install_tests_new "HotHORS" init_tests
