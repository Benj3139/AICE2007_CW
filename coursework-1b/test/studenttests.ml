open Util.Assert
open Gradedtests

let provided_tests : suite =
  [ Test
      ( "my custom LLVMlite test"
      , [ ( "interestingtestcase returns expected sum"
          , assert_eqf (fun () -> exec_e2e_file "llprograms/interestingtestcase.ll" "") 40L )
        ] )
  ]