(** Test module for HotRankedAlphabet. *)

open Test

module SSet = HotExtBatSet.StringSet

let printer set =
  let io = BatIO.output_string () in
    SSet.print BatIO.nwrite io set;
    BatIO.close_out io

(* Tests for union *)

let test_union_empty () =
  let out = SSet.union_all [] in
  let exp = SSet.empty in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

let test_union_non_empty () =
  let out = SSet.union_all [SSet.singleton "A"; SSet.singleton "B"] in
  let exp = SSet.add "A" (SSet.singleton "B") in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

let test_union_non_empty_overlapping () =
  let out = SSet.union_all [SSet.singleton "A"; SSet.singleton "A"] in
  let exp = SSet.singleton "A" in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

(* Tests for of_list *)

let test_of_list_empty () =
  let out = SSet.of_list [] in
  let exp = SSet.empty in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

let test_of_list_non_empty () =
  let out = SSet.of_list ["A";"B";] in
  let exp = SSet.add "A" (SSet.singleton "B") in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

let test_of_list_overlapping () =
  let out = SSet.of_list ["A";"B";"C";"A";"B"] in
  let exp = SSet.add "C" (SSet.add "A" (SSet.singleton "B")) in
    assert_equal ~printer:(printer) ~cmp:(SSet.equal) exp out

(* Tests for as_list *)

let test_as_list_empty () =
  let out = SSet.as_list SSet.empty in
  let exp = [] in
    assert_equal ~printer:(String.concat ";") exp out

let test_as_list_non_empty () =
  let lst = SSet.add "C" (SSet.add "A" (SSet.singleton "B")) in
  let out = BatList.sort (SSet.as_list lst) in
  let exp = ["A";"B";"C"] in
    assert_equal ~printer:(String.concat ";") exp out

let init_tests () =
  [
   ("union empty", test_union_empty);
   ("union non-empty", test_union_non_empty);
   ("union non-empty overlapping", test_union_non_empty_overlapping);
   ("of_list empty", test_of_list_empty);
   ("of_list non-empty", test_of_list_non_empty);
   ("of_list overlapping", test_of_list_overlapping);
   ("as_list empty", test_as_list_empty);
   ("as_list non-empty", test_as_list_non_empty);
  ]

let _ = install_tests_new "HotExtBatSet" init_tests
