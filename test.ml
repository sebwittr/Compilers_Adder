open Compile
open Runner
open Printf
open OUnit2

let t (name : string) (program : string) (expected : string) : OUnit2.test =
  name>::test_run program name expected;;
let te (name : string) (program : string) (expected_err : string) : OUnit2.test =
  name>::test_err program name expected_err;;

let suite : OUnit2.test =
"suite">:::
 [te "forty_one" "41" "not yet implemented";

  te "nyi" "(let ((x 10)) x)" "not yet implemented";

  ]
;;


let () =
  run_test_tt_main suite
;;
