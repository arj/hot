(** Test module for HotTerm. *)

open Batteries

open Test
open HotSimple.StringSortTerm
module HotPath = Path
open Path.Infix

module Sort = HotType.Sort
open Sort.Infix

let prnt p =
  let p' = BatOption.map (HotPath.string_of) p in
    BatOption.default "{None}" p'


let c0 = ("C", Sort.create_n 0)
let c1 = ("C", Sort.create_n 1)
let c2 = ("C", Sort.create_n 2)
let b1 = ("B", Sort.create_n 1)
let b0 = ("B", Sort.create_n 0)
let d1 = ("D", Sort.create_n 1)
let zero = ("zero",Sort.base)

(* path *)

let test_path_1 () =
  let exp = None in
  let out = Path.path (mkVar "y") "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_2 () =
  let exp = Some HotPath.epsilon in
  let out = Path.path (mkVar "x") "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_3 () =
  let exp = None in
  let out = Path.path (mkApp (mkCtor ("C",Sort.create_n 1)) [mkVar "y"]) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_4 () =
  let exp = Some(HotPath.Ele(c1,0,HotPath.Empty)) in
  let out = Path.path (mkApp (mkCtor c1) [mkVar "x"]) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_5 () =
  let exp = Some(HotPath.Ele(c1, 0,HotPath.Ele(c1,1,HotPath.Empty))) in
  let out = Path.path (mkApp (mkCtor c1)
                       [mkApp (mkCtor c1)
                              [mkApp (mkCtor c1) [mkVar "y"];
                               mkVar "x"]]) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_bottom () =
  let exp = None in
  let out = Path.path mkBottom "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_ctor () =
  let exp = None in
  let out = Path.path (mkCtor ("C",Sort.create_n 1)) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_var () =
  let exp = Some(HotPath.epsilon) in
  let out = Path.path (mkApp (mkVar "x") []) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_app_app_var () =
  let exp = Some(HotPath.epsilon) in
  let out = Path.path (mkApp(mkApp(mkApp (mkVar "x") []) []) []) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_bottom () =
  let exp = None in
  let out = Path.path (mkApp mkBottom []) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_app () =
  let exp = None in
  let out = Path.path (mkApp(mkApp mkBottom []) []) "x" in
    assert_equal ~printer:(prnt) exp out

(* subst_path *)

let test_subst_path_1 () =
  let exp = mkVar "x" in
  let out = Path.subst_path HotPath.epsilon (mkVar "x") mkBottom in
    assert_equal ~printer:string_of exp out

let test_subst_path_2 () =
  let exp = mkVar "x" in
  let out = Path.subst_path HotPath.epsilon (mkVar "x")
              (mkApp(mkCtor ("C",Sort.create_n 1)) [mkVar "y"]) in
    assert_equal ~printer:string_of exp out

let test_subst_path_3 () =
  let exp = mkVar "x" in
  let out = Path.subst_path HotPath.epsilon (mkVar "x")
              (mkVar "y") in
    assert_equal exp out

let test_subst_path_4 () =
  let f () = Path.subst_path (HotPath.Ele(("C", Sort.create_n 1),1,HotPath.epsilon)) mkBottom mkBottom in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_5 () =
  let f ()  = Path.subst_path (HotPath.Ele(("C", Sort.create_n 1),1,HotPath.epsilon)) mkBottom (mkVar "x") in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_6 () = 
  let f () = Path.subst_path (HotPath.Ele(("C",Sort.create_n 1),1,HotPath.epsilon)) mkBottom (mkApp (mkCtor ("A",Sort.create_n 1)) []) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_7 () =
  let exp = mkApp (mkCtor("C",Sort.create_n 1)) [mkBottom] in
  let out = Path.subst_path (HotPath.Ele(("C", Sort.create_n 1),0,HotPath.epsilon)) mkBottom (mkApp (mkCtor("C",Sort.create_n 1)) [mkVar "x"]) in
    assert_equal ~printer:string_of exp out

let test_subst_path_8 () =
  let f () = Path.subst_path (HotPath.Ele(("C",Sort.create_n 1), 1,HotPath.epsilon)) mkBottom (mkApp (mkCtor("C",Sort.create_n 1)) [mkVar "x"]) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_9 () =
  let c = ("C",Sort.create_n 3) in
  let d = ("D",Sort.create_n 3) in
  let e = ("E",Sort.create_n 1) in
  let inp = mkApp (mkCtor c) [mkVar "x";mkApp (mkCtor d) [mkVar "x";mkVar "t";mkApp (mkCtor e) [mkVar("q")]];mkVar("y")] in
  let exp = mkApp (mkCtor c) [mkVar "x";mkApp (mkCtor d) [mkVar "x";mkVar "t";mkApp (mkCtor e) [mkBottom]];mkVar("y")] in
  let path = HotPath.Ele(c,1,HotPath.Ele(d,2,HotPath.Ele(e,0,HotPath.epsilon))) in
  let out = Path.subst_path path mkBottom inp in
    assert_equal ~printer:string_of exp out
      
let test_subst_path_ctor_epsilon () =
  let exp = mkBottom in
  let out = Path.subst_path HotPath.epsilon mkBottom @@ mkCtor ("C",o) in
    assert_equal ~printer:string_of exp out

(* read *)

let test_read_1 () =
  let out = Path.read (mkVar "x") HotPath.Empty in
  let exp = mkVar "x" in
    assert_equal ~printer:string_of exp out

let test_read_2 () =
  let f () = Path.read (mkVar "x") (HotPath.Ele(c1,1,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_3 () =
  let out = Path.read (mkApp (mkCtor c1) [mkVar "x"]) (HotPath.Ele(c1,0,HotPath.Empty)) in
  let exp = mkVar "x" in
    assert_equal ~printer:string_of exp out

let test_read_4 () =
  let f () = Path.read (mkApp (mkCtor c1) [mkVar "x"]) (HotPath.Ele(c1,1,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_5 () =
  let f () = Path.read (mkApp (mkCtor d1) [mkVar "x"]) (HotPath.Ele(c1,0,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_6 () =
  let out = Path.read (mkApp (mkCtor c1) [mkVar "y"; mkApp (mkCtor c1) [mkVar "x"]]) (HotPath.Ele(c1,0,HotPath.Empty)) in
  let exp = mkVar "y" in
    assert_equal ~printer:string_of exp out

let test_read_7 () =
  let out = Path.read (mkApp (mkCtor c2) [mkVar "y";mkApp (mkCtor d1) [mkVar "x"]]) (HotPath.Ele(c2,1,HotPath.Ele(d1,0,HotPath.Empty))) in
  let exp = mkVar "x" in
    assert_equal ~printer:string_of exp out

(* TODO Shouldn't the first ctor be a c2? *)
let test_read_8 () =
  let f () = Path.read (mkApp (mkCtor c1) [mkVar "y";mkApp (mkCtor d1) [mkVar "x"]]) (HotPath.Ele(c1,1,HotPath.Ele(d1,1,HotPath.Empty))) in
    assert_raises HotTerm.Path_not_found_in_term f

(* Similar to test_read_8 but with infix syntax *)
let test_read_9 () =
  let f () = (mkApp (mkCtor c1) [mkVar "y";mkApp (mkCtor d1) [mkVar "x"]]) -. (HotPath.Ele(c1,1,HotPath.Ele(d1,1,HotPath.Empty))) in
    assert_raises HotTerm.Path_not_found_in_term f

(* subst *)

let test_subst_in_var () =
  let out = subst "x" mkBottom (mkVar "x") in
  let exp = mkBottom in
    assert_equal ~printer:(string_of) exp out

let test_subst_in_ctor () =
  let out = subst "x" mkBottom (mkApp (mkCtor c1) [mkVar "x"]) in
  let exp = mkApp (mkCtor c1) [mkBottom] in
    assert_equal ~printer:(string_of) exp out

let test_subst_not_present () = 
  let out = subst "x" mkBottom (mkApp (mkCtor c1) [mkVar "y"]) in
  let exp = mkApp (mkCtor c1) [mkVar "y"] in
    assert_equal ~printer:(string_of) exp out

let test_subst_app_var () =
  let exp = mkApp (mkCtor b1) [mkBottom] in
  let out = subst "x" (mkApp (mkCtor b1) []) (mkApp (mkVar "x") [mkBottom]) in
    assert_equal ~printer:(string_of ~show_type:true) exp out

(* string_of *)

let test_string_of_bottom () =
  let out = string_of mkBottom in
  let exp = "_|_" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_var () =
  let out = string_of (mkVar "x") in
  let exp = "x" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_ctor () =
  let out = string_of (mkApp (mkCtor c2) [mkVar "x";mkVar "y"]) in
  let exp = "C(x,y)" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_nullary_ctor () =
  let out = string_of (mkCtor c0) in
  let exp = "C" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_nullary_ctor_app () =
  let out = string_of (mkApp (mkCtor c0) []) in
  let exp = "C" in
    assert_equal ~printer:(fun x -> x) exp out

(* welldefined *)

let test_is_welldefined_bottom () =
  let out = is_welldefined mkBottom in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_var () =
  let out = is_welldefined (mkVar "x") in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_ctor_nonterminal () =
  let out = is_welldefined (mkCtor c1) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_ctor_terminal () =
  let out = is_welldefined (mkCtor ("a",o ^=> o)) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_var_no_args () =
  let out = is_welldefined (mkApp (mkVar "x") []) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_var_some_args () =
  let out = is_welldefined (mkApp (mkVar "x") [mkVar "y";mkBottom]) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_bottom () =
  let out = is_welldefined (mkApp mkBottom []) in
    assert_bool "Term should not be well-defined" (not out)

let test_is_welldefined_app_ctor_exact_no_of_args () =
  let out = is_welldefined
              (mkApp (mkCtor ("A",o ^=> o ^=> o))
                        [mkVar "x"; mkVar "x";mkBottom]) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_ctor_too_many_args () =
  let out = is_welldefined (mkApp (mkCtor ("A",o ^=> o ^=> o))
                                     [mkVar "x";mkVar "x";mkBottom;mkBottom]) in
    assert_bool "Term should not be well-defined" (not out)

let test_is_welldefined_app_ctor_less_args () =
  let out = is_welldefined
              (mkApp (mkCtor ("A",o ^=> o ^=> o))
                        [mkVar "x"]) in
    assert_bool "Term should be well-defined" out

(* depth *)

let test_depth_bottom () =
  let exp = 1 in
  let out = depth mkBottom in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_var () =
  let exp = 1 in
  let out = depth (mkVar "x") in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app () =
  let exp = 1 in
  let out = depth (mkApp (mkCtor ("b",o)) []) in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app_bottom () =
  let exp = 2 in
  let out = depth (mkApp (mkCtor ("b",o)) [mkBottom]) in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app_app_bottom () =
  let exp = 3 in
  let out = depth (mkApp (mkCtor ("b",o)) [mkBottom;mkApp (mkCtor ("b",o)) [mkBottom]]) in
    assert_equal ~printer:(string_of_int) exp out

(* unify *)

let env_pair_pr (x,t) =
  Printf.sprintf "%s -> %s" x (string_of t)

let env_pr lst =
  let s = String.concat "," @@ BatList.map env_pair_pr lst in
    Printf.sprintf "[%s]" s

let ures_pr res = match res with
  | Ok(lst) -> env_pr lst
  | Bad(_) -> "Bad"

let test_unify_ctor_ctor () =
  let exp = Ok([]) in
  let out = unify (mkCtor c1) (mkCtor c1) in
    assert_equal ~printer:ures_pr exp out

let test_unify_ctor_var () =
  let exp = Ok([("x",mkCtor c1)]) in
  let out = unify (mkCtor c1) (mkVar "x") in
    assert_equal ~printer:ures_pr exp out

let test_unify_var_ctor () =
  let exp = Ok([("x",mkCtor c1)]) in
  let out = unify (mkVar "x") (mkCtor c1) in
    assert_equal ~printer:ures_pr exp out

let test_unify_bottom_bottom () =
  let exp = Ok([]) in
  let out = unify mkBottom mkBottom in
    assert_equal ~printer:ures_pr exp out

let test_unify_bottom_ctor () =
  let exp = Ok([]) in
  let out = unify mkBottom @@ mkCtor c1 in
    assert_equal ~printer:ures_pr exp out

let test_unify_app_app_fst_ok () =
  let exp = Ok([]) in
  let out = unify (mkApp (mkCtor c1) []) (mkApp (mkCtor c1) []) in
    assert_equal ~printer:ures_pr exp out

let test_unify_app_app_fst_bad () =
  let exp = Bad(()) in
  let out = unify (mkApp (mkCtor c1) []) (mkApp (mkCtor c2) []) in
    assert_equal ~printer:ures_pr exp out

let test_unify_app_app_snd_ok () =
  let exp = [("x",mkCtor c1);("y",mkCtor d1)] in
  let out = unify (mkApp (mkCtor c1) [mkVar "x";mkCtor d1]) (mkApp (mkCtor c1) [mkCtor c1;mkVar "y"]) in
    match out with
      | Ok(lst) -> assert_equal_list ~printer:env_pair_pr exp lst
      | Bad(_) -> assert_failure "Expected Ok but got Bad"

let test_unify_app_app_snd_bad_1 () =
  let exp = Bad(()) in
  let out = unify (mkApp (mkCtor c1) [mkVar "x";mkCtor d1]) (mkApp (mkCtor c1) [mkCtor c1;mkCtor c1]) in
    assert_equal ~printer:ures_pr exp out

let test_unify_app_app_snd_bad_2 () =
  let exp = Bad(()) in
  let out = unify (mkApp (mkCtor c1) [mkCtor d1;mkCtor d1]) (mkApp (mkCtor c1) [mkCtor c1;mkCtor d1]) in
    assert_equal ~printer:ures_pr exp out

let test_unify_app_app_diff_length () =
  let exp = Bad(()) in
  let out = unify (mkApp (mkCtor c1) [mkCtor d1;mkCtor d1]) (mkApp (mkCtor c1) [mkCtor d1]) in
    assert_equal ~printer:ures_pr exp out

let test_unify_zero_star () =
  let exp = Ok(["*",mkCtor zero]) in
  let out = unify (mkCtor zero) @@ mkVar "*" in
    assert_equal ~printer:ures_pr exp out

let test_unify_appzero_star () =
  let exp = Ok(["*",mkCtor zero]) in
  let out = unify (mkApp (mkCtor zero) []) @@ mkVar "*" in
    assert_equal ~printer:ures_pr exp out

(* remove_suffix *)

let test_remove_suffix_empty_path_empty_suffix () =
  let open Path in
  let exp = Empty in
  let out = remove_suffix exp Empty in
    assert_equal ~printer:string_of exp out

let test_remove_suffix_nonempty_path_empty_suffix () =
  let open Path in
  let exp = Ele(c0,0,Ele(c0,0,Empty)) in
  let out = remove_suffix exp Empty in
    assert_equal ~printer:string_of exp out

let test_remove_suffix_nonempty_path_nonempty_suffix () =
  let open Path in
  let exp = Ele(b0,0,Empty) in
  let inp = Ele(b0,0,Ele(c0,0,Empty)) in
  let suf = Ele(c0,0,Empty) in
  let out = remove_suffix inp suf in
    assert_equal ~printer:string_of exp out

let test_remove_suffix_nonempty_path_nonempty_suffix_bad () =
  let open Path in
  let inp = Ele(b0,0,Empty) in
  let suf = Ele(c0,0,Empty) in
  let out () = remove_suffix inp suf in
    assert_raises HotTerm.Path_not_found_in_term out

let test_remove_suffix_nonempty_path_nonempty_suffix_bad_index () =
  let open Path in
  let inp = Ele(b0,0,Empty) in
  let suf = Ele(b0,1,Empty) in
  let out () = remove_suffix inp suf in
    assert_raises HotTerm.Path_not_found_in_term out

let test_remove_suffix_suffix_too_long () =
  let open Path in
  let inp = Ele(c0,0,Empty) in
  let suf = Ele(c0,0,Ele(c0,0,Empty)) in
  let out () = remove_suffix inp suf in
    assert_raises HotTerm.Path_not_found_in_term out

(* is_suffix *)

let test_is_suffix_true () =
  let open Path in
  let inp = Ele(b0,0,Ele(c0,0,Empty)) in
  let suf = Ele(c0,0,Empty) in
    assert_bool "is_suffix should be true" @@ is_suffix inp suf

let test_is_suffix_false () =
  let open Path in
  let inp = Ele(c0,0,Empty) in
  let suf = Ele(c0,0,Ele(c0,0,Empty)) in
    assert_bool "is_suffix should be false" @@ not @@ is_suffix inp suf

(* TODO Rename tests. Numbered tests are not meaningful... *)
let init_tests () =
  [
   ("subst_path 1", test_subst_path_1);
   ("subst_path 2", test_subst_path_2);
   ("subst_path 3", test_subst_path_3);
   ("subst_path 4", test_subst_path_4);
   ("subst_path 5", test_subst_path_5);
   ("subst_path 6", test_subst_path_6);
   ("subst_path 7", test_subst_path_7);
   ("subst_path 8", test_subst_path_8);
   ("subst_path 9", test_subst_path_9);
   ("subst_path ctor epsilon", test_subst_path_ctor_epsilon);
   ("path 1", test_path_1);
   ("path 2", test_path_2);
   ("path 3", test_path_3);
   ("path 4", test_path_4);
   ("path 5", test_path_5);
   ("path bottom", test_path_bottom);
   ("path ctor", test_path_ctor);
   ("path app var", test_path_app_var);
   ("path app app app var", test_path_app_app_app_var);
   ("path app bottom", test_path_app_bottom);
   ("path app app", test_path_app_app);
   ("read 1", test_read_1);
   ("read 2", test_read_2);
   ("read 3", test_read_3);
   ("read 4", test_read_4);
   ("read 5", test_read_5);
   ("read 6", test_read_6);
   ("read 7", test_read_7);
   ("read 8", test_read_8);
   ("read 9", test_read_9);
   ("subst in var", test_subst_in_var);
   ("subst in ctor", test_subst_in_ctor);
   ("subst not present", test_subst_not_present);
   ("subst app var", test_subst_app_var);
   ("string_of bottom", test_string_of_bottom);
   ("string_of var", test_string_of_var);
   ("string_of ctor", test_string_of_ctor);
   ("string_of 0-ary ctor", test_string_of_nullary_ctor);
   ("string_of 0-ary ctor app", test_string_of_nullary_ctor_app);
(*   ("is_welldefined bottom", test_is_welldefined_bottom); (* TODO *)
   ("is_welldefined var", test_is_welldefined_var);
   ("is_welldefined ctor non-terminal", test_is_welldefined_ctor_nonterminal);
   ("is_welldefined ctor terminal", test_is_welldefined_ctor_terminal);
   ("is_welldefined app var no arguments", test_is_welldefined_app_var_no_args);
   ("is_welldefined app var some arguments", test_is_welldefined_app_var_some_args);
   ("is_welldefined app bottom", test_is_welldefined_app_bottom);
   ("is_welldefined app ctor exact number of args", test_is_welldefined_app_ctor_exact_no_of_args);
   ("is_welldefined app ctor too many args", test_is_welldefined_app_ctor_too_many_args);
   ("is_welldefined app ctor less args", test_is_welldefined_app_ctor_less_args);*)
   ("depth bottom", test_depth_bottom);
   ("depth var", test_depth_var);
   ("depth app", test_depth_app);
   ("depth app bottom", test_depth_app_bottom);
   ("depth app bottom (app bottom)", test_depth_app_app_bottom);
   ("unify ctor,ctor", test_unify_ctor_ctor);
   ("unify ctor,var", test_unify_ctor_var);
   ("unify var,ctor", test_unify_var_ctor);
   ("unify bottom,bottom", test_unify_bottom_bottom);
   ("unify bottom,ctor", test_unify_bottom_ctor);
   ("unify app,app fst ok", test_unify_app_app_fst_ok);
   ("unify app,app fst bad", test_unify_app_app_fst_bad);
   ("unify app,app snd ok", test_unify_app_app_snd_ok);
   ("unify app,app snd bad 1", test_unify_app_app_snd_bad_1);
   ("unify app,app snd bad 2", test_unify_app_app_snd_bad_2);
   ("unify app,app diff length", test_unify_app_app_diff_length);
   ("unify zero,*", test_unify_zero_star);
   ("unify (zero) [],*", test_unify_appzero_star);
   ("path remove_suffix empty empty", test_remove_suffix_empty_path_empty_suffix);
   ("path remove_suffix nonempty empty", test_remove_suffix_nonempty_path_empty_suffix);
   ("path remove_suffix nonempty nonempty", test_remove_suffix_nonempty_path_nonempty_suffix);
   ("path remove_suffix nonempty nonempty bad", test_remove_suffix_nonempty_path_nonempty_suffix_bad);
   ("path remove_suffix nonempty nonempty bad index", test_remove_suffix_nonempty_path_nonempty_suffix_bad_index);
   ("path remove_suffix suffix too long", test_remove_suffix_suffix_too_long);
   ("path is_suffix true", test_is_suffix_true);
   ("path is_suffix false", test_is_suffix_false);
  ]

let _ = install_tests_new "HotTerm" init_tests
