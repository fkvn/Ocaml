(* Main program:  runs our tests *)


(* switch EvalEnv to EvalSubst to test substitution-based interpreter *)

module Eval = EvalEnv

(* module Eval = EvalSubst *)

let main = Testing.run_tests Eval.eval Testing.tests

(* let main = Testing.run_tests Eval.debug_eval Testing.tests *)
