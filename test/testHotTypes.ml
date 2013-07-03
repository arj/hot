(** Test module for HotTypes. *)

open Test

module Sort = HotTypes.Sort

let test_order_1 () =
  let out = Sort.order Sort.base in
  let exp = 0 in
    assert_equal ~printer:(string_of_int) exp out

let test_order_2 () =
  let out = Sort.order (Sort.create_n 10) in
  let exp = 1 in
    assert_equal ~printer:(string_of_int) exp out

let test_order_3 () =
  let t = Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base)) in
  let out = Sort.order t in
  let exp = 1 in
    assert_equal ~printer:(string_of_int) exp out

let test_order_4 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.base,Sort.base),Sort.base) in
  let out = Sort.order t in
  let exp = 2 in
    assert_equal ~printer:(string_of_int) exp out

let test_order_5 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.Arrow(Sort.base,Sort.base),Sort.base),Sort.base) in
  let out = Sort.order t in
  let exp = 3 in
    assert_equal ~printer:(string_of_int) exp out

let test_order_6 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base)),Sort.base) in
  let out = Sort.order t in
  let exp = 2 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_1 () =
  let out = Sort.arity Sort.base in
  let exp = 0 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_2 () =
  let out = Sort.arity (Sort.create_n 10) in
  let exp = 10 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_3 () =
  let t = Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base)) in
  let out = Sort.arity t in
  let exp = 2 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_4 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.base,Sort.base),Sort.base) in
  let out = Sort.arity t in
  let exp = 1 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_5 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.Arrow(Sort.base,Sort.base),Sort.base),Sort.base) in
  let out = Sort.arity t in
  let exp = 1 in
    assert_equal ~printer:(string_of_int) exp out

let test_arity_6 () =
  let t = Sort.Arrow(Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base)),Sort.base) in
  let out = Sort.arity t in
  let exp = 1 in
    assert_equal ~printer:(string_of_int) exp out

let test_create_n_1 () =
  let out = Sort.create_n 0 in
  let exp = Sort.base in
    assert_equal ~printer:(Sort.string_of) exp out

let test_create_n_2 () =
  let f () = Sort.create_n (-1) in
    assert_raises (Failure "Argument negative") f

let test_create_n_3 () =
  let out = Sort.create_n 2 in
  let exp = Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base)) in
    assert_equal ~printer:(Sort.string_of) exp out

let test_string_of_1 () =
  let out = Sort.string_of (Sort.base) in
  let exp = "o" in
    assert_equal exp out

let test_string_of_2 () =
  let out = Sort.string_of (Sort.Arrow(Sort.base,Sort.Arrow(Sort.base,Sort.base))) in
  let exp = "(o -> (o -> o))" in
    assert_equal exp out

let test_string_of_3 () =
  let out = Sort.string_of (Sort.Arrow(Sort.Arrow(Sort.base,Sort.base),Sort.base)) in
  let exp = "((o -> o) -> o)" in
    assert_equal exp out

let init_tests () =
  [
   ("order 1", test_order_1);
   ("order 2", test_order_2);
   ("order 3", test_order_3);
   ("order 4", test_order_4);
   ("order 5", test_order_5);
   ("order 6", test_order_6);
   ("arity 1", test_arity_1);
   ("arity 2", test_arity_2);
   ("arity 3", test_arity_3);
   ("arity 4", test_arity_4);
   ("arity 5", test_arity_5);
   ("arity 6", test_arity_6);
   ("create_n 1", test_create_n_1);
   ("create_n 2", test_create_n_2);
   ("create_n 3", test_create_n_3);
   ("string_of 1", test_string_of_1);
   ("string_of 2", test_string_of_2);
   ("string_of 3", test_string_of_3);
  ]

let _ = install_tests_new "HotType" init_tests
