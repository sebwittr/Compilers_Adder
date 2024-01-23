open Compile
open Runner
open Printf
open OUnit2

(* Runs a program, given as a source string, and compares its output to expected *)
let t (name : string) (program : string) (expected : string) : OUnit2.test =
  name >:: test_run program name expected
;;

(* Runs a program, given as a source string, and compares its error to expected *)
let te (name : string) (program : string) (expected_err : string) : OUnit2.test =
  name >:: test_err program name expected_err
;;

(* Runs a program, given as the name of a file in the input/ directory, and compares its output to expected *)
let ti (filename : string) (expected : string) = filename >:: test_run_input filename expected

(* Runs a program, given as the name of a file in the input/ directory, and compares its error to expected *)
let tie (filename : string) (expected_err : string) =
  filename >:: test_err_input filename expected_err
;;

let suite : OUnit2.test =
  "suite"
  >::: [
    te "multiple" "0 1" "Multiple top-level s-expressions found";
    te "mt" "" "Multiple top-level s-expressions found";
    te "mt_call" "()" "Unexpected empty expression at line 0, col 0";

    te "invalid_call1" "(abc)" "Invalid call at line 0, col 0";
    te "invalid_call2" "(add1)" "Invalid call at line 0, col 0";
    te "invalid_call3" "(add1 1 2)" "Invalid call at line 0, col 0";
    te "invalid_call4" "(let (x 1))" "Invalid call at line 0, col 0";
    te "invalid_call5" "(let (x 1) 1 2)" "Invalid call at line 0, col 0";

    te "t_bool" "true" "Unexpected boolean literal at line 0, col 0";
    te "f_bool" "false" "Unexpected boolean literal at line 0, col 0";

    te "bad_binding1" "(let (abc) 1)" "Bad expression in let at line 0, col 6";
    te "bad_binding2" "(let (()) 1)" "Bad expression in let at line 0, col 6";
    te "bad_binding3" "(let ((x)) 1)" "Bad expression in let at line 0, col 6";
    te "bad_binding4" "(let ((x 1 2)) 1)" "Bad expression in let at line 0, col 6";
    
    te "unb0" "x" "Unbound identifier x";
    te "unb1" "(let ((x 1)) y)" "Unbound identifier y at line 0, col 13";
    te "unb2" "(add1 (sub1 y))" "Unbound identifier y at line 0, col 12";

    t "num" "0" "0";
    t "add" "(add1 -2)" "-1";
    t "sub" "(sub1 10)" "9";

    t "let1" "(let ((x 1)) x)" "1";
    t "let2" "(let ((y 2) (x 1)) (add1 x))" "2";
    t "let3" "(let ((x 1) (y 2) (z 3)) 10)" "10";
    t "let4" "(let ((y 2) (x (sub1 y))) x)" "1";
    t "let_nest" "(let ((x 1))
                    (let ((x (add1 x))) x))" "2";

    te "let_mt" "(let () 1)" "Unexpected empty bindings at line 0, col 5";
    te "let_unb" "(let ((x (sub1 y)) (y 2)) x)" "Unbound identifier y at line 0, col 15";
    te "let_dup" "(let ((x 1) (x 2)) 3)" "Duplicate binding for x in let expression at line 0, col 0";
    
    t "neg" "(sub1 0)" "-1";
  ]
;;

let () = run_test_tt_main suite
