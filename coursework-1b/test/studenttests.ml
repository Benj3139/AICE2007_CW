open Util.Assert
open Gradedtests

<<<<<<< HEAD
(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests = []
(*
let provided_tests : suite = [
    Test ("my custom LLVMlite test", fun () ->
    run_file "llprograms/interestingtestcase.ll"
  )
]
*)

=======
let provided_tests : suite =
  [ Test
      ( "my custom LLVMlite test"
      , [ ( "interestingtestcase returns expected sum"
          , assert_eqf (fun () -> exec_e2e_file "llprograms/interestingtestcase.ll" "") 40L )
        ] )
  ]
>>>>>>> edd3c285d6ac55d1e284458d31bf6bf34f720421
