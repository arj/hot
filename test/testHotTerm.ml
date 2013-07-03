(** Test module for HotTerm. *)

open Test
open HotTerm.SortTerm
open HotTerm.SortTerm.Infix

module Sort = HotTypes.Sort
open HotTypes.Sort.Infix

let prnt p =
  let p' = BatOption.map (HotPath.string_of) p in
    BatOption.default "{None}" p'

(* path *)

let test_path_1 () =
  let exp = None in
  let out = path (Var("y")) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_2 () =
  let exp = Some HotPath.epsilon in
  let out = path (Var("x")) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_3 () =
  let exp = None in
  let out = path (App(Ctor("C",Sort.create_n 1), [Var("y")])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_4 () =
  let exp = Some(HotPath.Ele("C",0,HotPath.Empty)) in
  let out = path (App(Ctor("C",Sort.create_n 1), [Var("x")])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_5 () =
  let exp = Some(HotPath.Ele("C",0,HotPath.Ele("C",1,HotPath.Empty))) in
  let out = path (App(Ctor("C",Sort.create_n 1),
                       [(App(Ctor("C",Sort.create_n 1),
                              [(App(Ctor("C",Sort.create_n 1), [Var("y")]));
                               Var("x")]))])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_bottom () =
  let exp = None in
  let out = path Bottom "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_ctor () =
  let exp = None in
  let out = path (Ctor("C",Sort.create_n 1)) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_var () =
  let exp = Some(HotPath.epsilon) in
  let out = path (App(Var("x"),[])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_app_app_var () =
  let exp = Some(HotPath.epsilon) in
  let out = path (App(App(App(Var("x"),[]),[]),[])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_bottom () =
  let exp = None in
  let out = path (App(Bottom,[])) "x" in
    assert_equal ~printer:(prnt) exp out

let test_path_app_app () =
  let exp = None in
  let out = path (App(App(Bottom,[]),[])) "x" in
    assert_equal ~printer:(prnt) exp out

(* subst_path *)

let test_subst_path_1 () =
  let exp = Var("x") in
  let out = subst_path HotPath.epsilon (Var("x")) Bottom in
    assert_equal exp out

let test_subst_path_2 () =
  let exp = Var("x") in
  let out = subst_path HotPath.epsilon (Var("x"))
              (App(Ctor("C",Sort.create_n 1), [Var("y")])) in
    assert_equal exp out

let test_subst_path_3 () =
  let exp = Var("x") in
  let out = subst_path HotPath.epsilon (Var("x"))
              (Var("y")) in
    assert_equal exp out

let test_subst_path_4 () =
  let f () = subst_path (HotPath.Ele("C",1,HotPath.epsilon)) Bottom Bottom in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_5 () =
  let f ()  = subst_path (HotPath.Ele("C",1,HotPath.epsilon)) Bottom (Var("x")) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_6 () = 
  let f () = subst_path (HotPath.Ele("C",1,HotPath.epsilon)) Bottom (App(Ctor("A",Sort.create_n 1), [])) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_7 () =
  let exp = App(Ctor("C",Sort.create_n 1),[Bottom]) in
  let out = subst_path (HotPath.Ele("C",0,HotPath.epsilon)) Bottom (App(Ctor("C",Sort.create_n 1), [Var("x")])) in
    assert_equal ~printer:string_of exp out

let test_subst_path_8 () =
  let f () = subst_path (HotPath.Ele("C",1,HotPath.epsilon)) Bottom (App(Ctor("C",Sort.create_n 1), [Var("x")])) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_subst_path_9 () =
  let inp = App(Ctor("C",Sort.create_n 3),[Var("x");App(Ctor("D",Sort.create_n 3),[Var("x");Var("t");App(Ctor("C",Sort.create_n 1),[Var("q")])]);Var("y")]) in
  let exp = App(Ctor("C",Sort.create_n 3),[Var("x");App(Ctor("D",Sort.create_n 3),[Var("x");Var("t");App(Ctor("C",Sort.create_n 1),[Bottom])]);Var("y")]) in
  let path = HotPath.Ele("C",1,HotPath.Ele("D",2,HotPath.Ele("C",0,HotPath.epsilon))) in
  let out = subst_path path Bottom inp in
    assert_equal ~printer:string_of exp out
      
let test_subst_path_ctor_epsilon () =
  let exp = Bottom in
  let out = subst_path HotPath.epsilon Bottom (Ctor("C",o)) in
    assert_equal exp out

(* read *)

let test_read_1 () =
  let out = read (Var("x")) HotPath.Empty in
  let exp = Var("x") in
    assert_equal ~printer:string_of exp out

let test_read_2 () =
  let f () = read (Var("x")) (HotPath.Ele("C",1,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_3 () =
  let out = read (App(Ctor("C",Sort.create_n 1),[Var("x")])) (HotPath.Ele("C",0,HotPath.Empty)) in
  let exp = Var("x") in
    assert_equal ~printer:string_of exp out

let test_read_4 () =
  let f () = read (App(Ctor("C",Sort.create_n 1),[Var("x")])) (HotPath.Ele("C",1,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_5 () =
  let f () = read (App(Ctor("D",Sort.create_n 1),[Var("x")])) (HotPath.Ele("C",0,HotPath.Empty)) in
    assert_raises HotTerm.Path_not_found_in_term f

let test_read_6 () =
  let out = read (App(Ctor("C",Sort.create_n 1),[Var("y");App(Ctor("D",Sort.create_n 1),[Var("x")])])) (HotPath.Ele("C",0,HotPath.Empty)) in
  let exp = Var("y") in
    assert_equal ~printer:string_of exp out

let test_read_7 () =
  let out = read (App(Ctor("C",Sort.create_n 2),[Var("y");App(Ctor("D",Sort.create_n 1),[Var("x")])])) (HotPath.Ele("C",1,HotPath.Ele("D",0,HotPath.Empty))) in
  let exp = Var("x") in
    assert_equal ~printer:string_of exp out

let test_read_8 () =
  let f () = read (App(Ctor("C",Sort.create_n 1),[Var("y");App(Ctor("D",Sort.create_n 1),[Var("x")])])) (HotPath.Ele("C",1,HotPath.Ele("D",1,HotPath.Empty))) in
    assert_raises HotTerm.Path_not_found_in_term f

(* Similar to test_read_8 but with infix syntax *)
let test_read_9 () =
  let f () = (App(Ctor("C",Sort.create_n 1),[Var("y");App(Ctor("D",Sort.create_n 1),[Var("x")])])) -. (HotPath.Ele("C",1,HotPath.Ele("D",1,HotPath.Empty))) in
    assert_raises HotTerm.Path_not_found_in_term f

(* subst *)

let test_subst_in_var () =
  let out = subst "x" Bottom (Var("x")) in
  let exp = Bottom in
    assert_equal ~printer:(string_of) exp out

let test_subst_in_ctor () =
  let out = subst "x" Bottom (App(Ctor("C",Sort.create_n 1), [Var("x")])) in
  let exp = App(Ctor("C",Sort.create_n 1), [Bottom]) in
    assert_equal ~printer:(string_of) exp out

let test_subst_not_present () = 
  let out = subst "x" Bottom (App(Ctor("C",Sort.create_n 1), [Var("y")])) in
  let exp = App(Ctor("C",Sort.create_n 1), [Var("y")]) in
    assert_equal ~printer:(string_of) exp out

let test_subst_app_var () =
  let exp = App(Ctor("b",o ^=> o),[Bottom]) in
  let out = subst "x" (App(Ctor("b",o ^=> o),[])) (App(Var("x"),[Bottom])) in
    assert_equal ~printer:(string_of ~sort:true) exp out

(* string_of *)

let test_string_of_bottom () =
  let out = string_of Bottom in
  let exp = "_|_" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_var () =
  let out = string_of (Var("x")) in
  let exp = "x" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_ctor () =
  let out = string_of (App(Ctor("C",Sort.create_n 2), [Var("x");Var("y")])) in
  let exp = "C(x,y)" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_nullary_ctor () =
  let out = string_of (Ctor("C", Sort.create_n 0)) in
  let exp = "C" in
    assert_equal ~printer:(fun x -> x) exp out

let test_string_of_nullary_ctor_app () =
  let out = string_of (App(Ctor("C", Sort.create_n 0), [])) in
  let exp = "C" in
    assert_equal ~printer:(fun x -> x) exp out

(* welldefined *)

let test_is_welldefined_bottom () =
  let out = is_welldefined Bottom in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_var () =
  let out = is_welldefined (Var("x")) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_ctor_nonterminal () =
  let out = is_welldefined (Ctor("C",o ^=> o)) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_ctor_terminal () =
  let out = is_welldefined (Ctor("a",o ^=> o)) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_var_no_args () =
  let out = is_welldefined (App(Var("x"),[])) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_var_some_args () =
  let out = is_welldefined (App(Var("x"),[Var("y");Bottom])) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_bottom () =
  let out = is_welldefined (App(Bottom, [])) in
    assert_bool "Term should not be well-defined" (not out)

let test_is_welldefined_app_ctor_exact_no_of_args () =
  let out = is_welldefined
              (App(Ctor("A",o ^=> o ^=> o),
                        [Var("x");Var("x");Bottom])) in
    assert_bool "Term should be well-defined" out

let test_is_welldefined_app_ctor_too_many_args () =
  let out = is_welldefined (App(Ctor("A",o ^=> o ^=> o),
                                     [Var("x");Var("x");Bottom;Bottom])) in
    assert_bool "Term should not be well-defined" (not out)

let test_is_welldefined_app_ctor_less_args () =
  let out = is_welldefined
              (App(Ctor("A",o ^=> o ^=> o),
                        [Var("x")])) in
    assert_bool "Term should be well-defined" out

(* depth *)

let test_depth_bottom () =
  let exp = 1 in
  let out = depth Bottom in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_var () =
  let exp = 1 in
  let out = depth (Var("x")) in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app () =
  let exp = 1 in
  let out = depth (App(Ctor("b",o),[])) in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app_bottom () =
  let exp = 2 in
  let out = depth (App(Ctor("b",o),[Bottom])) in
    assert_equal ~printer:(string_of_int) exp out

let test_depth_app_app_bottom () =
  let exp = 3 in
  let out = depth (App(Ctor("b",o),[Bottom;App(Ctor("b",o),[Bottom])])) in
    assert_equal ~printer:(string_of_int) exp out

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
  ]

let _ = install_tests_new "HotTerm" init_tests
