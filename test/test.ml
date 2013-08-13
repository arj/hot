include OUnit
(*open ProglangUtils*)

open Batteries

type tests = (string * test_fun) list

let assert_equal_list ?cmp ?printer exp out =
  let c = match cmp with
    | Some(cmp) -> cmp
    | None -> compare
  in
  let exp' = BatList.sort c exp in
  let out' = BatList.sort c out in
  let comparison = BatList.map2 c exp' out' in
  let txt = "Lists are not equivalent" in
    if BatList.exists (fun i -> i <> 0) comparison then
      match printer with
        | Some(p) ->
            let txt_exp = String.concat "," @@ List.map p exp in
            let txt_out = String.concat "," @@ List.map p out in
              assert_failure @@ Printf.sprintf "%s [%s] [%s] " txt txt_exp txt_out
        | None -> assert_failure txt

(*let assert_result_equal ?cmp ?printer exp out =
  let comparator =
    match cmp with
      | Some(c) -> c
      | None -> compare
  in
    match exp,out with
      | Ok(exp'),Ok(out') ->
          comparator exp out
      | Bad(exp'),Bad(out') ->
      | _ -> assert_failure "Results are not equivalent" (* TODO Add printer:
                                                          result -> string *)
 *)

let g_all_tests : (string * (unit -> tests)) list ref = ref []
  
let install_tests_new name tests =
  g_all_tests := !g_all_tests @ [(name,tests)]

let is_good l = 
  match l with
    | None -> (fun _ -> true)
    | Some l -> (fun (name,_) -> List.mem name l)
  
let run_tests l =
  (* adds the test of a module into the the tests *)
  let add_tests tests (name,t_f) =
    if (is_good l (name,t_f)) then
      (* init tests *)
      let t_tests = t_f () in
      (* convert for OUnit *)
      let t = 
        TestLabel(name,
                  TestList (List.map 
                              (fun (n,f) -> TestLabel(n, TestCase f))
                              t_tests))
      in 
        tests @ [t]
    else
      tests
  in
  (* collect all tests that should be run, inizialize them *)
  let all_tests = 
    List.fold_left 
      add_tests 
      []
      (!g_all_tests)
  in
  let res = run_test_tt ~verbose:true (TestList all_tests) in
  let is_success = function
    | RSuccess _ -> true
    | _ -> false
  in
    List.for_all is_success res
