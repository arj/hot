(** Test module for HotRankedAlphabet. *)

open Test

module Sort = HotTypes.Sort
module SRA = HotRankedAlphabet.StringRankedAlphabet

let printer set =
  let io = BatIO.output_string () in
    SRA.print
      (fun io (s,sort) -> BatIO.nwrite io s)
      io set;
    BatIO.close_out io

(* Order tests *)

let test_order_empty () =
  let out = SRA.order SRA.empty in
  let exp = -1 in
    assert_equal ~printer:(string_of_int) out exp

let test_order () =
  let out = SRA.order (SRA.singleton ("F", Sort.base)) in
  let exp = 0 in
    assert_equal ~printer:(string_of_int) out exp

(* String_of tests *)

let test_string_of_empty () =
  let out = SRA.string_of SRA.empty in
  let exp = "{}" in
    assert_equal out exp

let test_string_of () =
  let out = SRA.string_of (SRA.singleton ("F", Sort.base)) in
  let exp = "{F:o}" in (* TODO pull out string_of sort! *)
    assert_equal out exp

let init_tests () =
  [
    ("order empty", test_order_empty);
    ("order", test_order);
    ("string_of empty", test_string_of_empty);
    ("string_of", test_string_of);
  ]

let _ = install_tests_new "HotRankedAlphabet" init_tests
