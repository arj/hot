open OUnit

let () = Printexc.record_backtrace true

let tests () =
  ignore (Test.run_tests None);;

(* Setting logger appropriately *)
let _ = BatLogger.init [("HotHORS", BatLogger.DEBUG)] BatLogger.stderr_formatter;;

tests ()
