(** Test module for HotPath. *)

open Test

let path1 = HotPath.Ele("C", 1, HotPath.Empty)
let path2 = HotPath.Ele("D", 2, HotPath.Empty)
let path1_2 = HotPath.Ele("C", 1, HotPath.Ele("D", 2, HotPath.Empty))
let path2_1 = HotPath.Ele("D", 2, HotPath.Ele("C", 1, HotPath.Empty))

(* epsilon *)

let test_epsilon () =
  let out = HotPath.epsilon in
  let exp = HotPath.Empty in
    assert_equal ~printer:(HotPath.string_of) exp out

(* append *)

let test_append_epsilon () =
  let out = HotPath.append HotPath.epsilon HotPath.epsilon in
  let exp = HotPath.Empty in
    assert_equal ~printer:(HotPath.string_of) exp out

let test_append_on_epsilon_1 () =
  let out = HotPath.append HotPath.epsilon path1 in
  let exp = path1 in
    assert_equal ~printer:(HotPath.string_of) exp out

let test_append_on_epsilon_2 () =
  let out = HotPath.append path1 HotPath.epsilon in
  let exp = path1 in
    assert_equal ~printer:(HotPath.string_of) exp out

let test_append () =
  let out = HotPath.append path1 path2 in
  let exp = path1_2 in
    assert_equal ~printer:(HotPath.string_of) exp out

(* reverse *)

let test_reverse_epsilon () =
  let out = HotPath.reverse HotPath.epsilon in
  let exp = HotPath.epsilon in
    assert_equal ~printer:(HotPath.string_of) exp out

let test_reverse () =
  let out = HotPath.reverse path1_2 in
  let exp = path2_1 in
    assert_equal ~printer:(HotPath.string_of) exp out

(* is_empty *)

let test_is_empty_epsilon () =
  let out = HotPath.is_empty HotPath.epsilon in
    assert_bool "is_empty should be true on epsilon path" out

let test_is_empty () =
  let out = HotPath.is_empty path1 in
    assert_bool "is_empty should be false on non-empty path" (not out)

(* string_of *)

let test_string_of_epsilon_1 () =
  let out = HotPath.string_of HotPath.epsilon in
  let exp = "" in
    assert_equal exp out

let test_string_of_epsilon_2 () =
  let out = HotPath.string_of ~epsilon:true HotPath.epsilon in
  let exp = "{epsilon}" in
    assert_equal exp out

let test_string_of_1 () =
  let out = HotPath.string_of path1 in
  let exp = "C.1" in
    assert_equal exp out

let test_string_of_2 () =
  let out = HotPath.string_of ~epsilon:true path1 in
  let exp = "C.1" in
    assert_equal exp out

let test_string_of_3 () =
  let out = HotPath.string_of ~epsilon:false path1 in
  let exp = "C.1" in
    assert_equal exp out

let init_tests () =
  [
   ("epsilon", test_epsilon);
   ("append epsilon", test_append_epsilon);
   ("append on epsilon 1", test_append_on_epsilon_1);
   ("append on epsilon 2", test_append_on_epsilon_2);
   ("append", test_append);
   ("reverse epsilon", test_reverse_epsilon);
   ("reverse", test_reverse);
   ("is_empty epsilon", test_is_empty_epsilon);
   ("is_empty", test_is_empty);
   ("string_of epsilon 1", test_string_of_epsilon_1);
   ("string_of epsilon 2", test_string_of_epsilon_2);
   ("string_of (?epsilon:default)", test_string_of_1);
   ("string_of (?epsilon:true)", test_string_of_2);
   ("string_of (?epsilon:false)", test_string_of_3);
  ]

let _ = install_tests_new "HotPath" init_tests
