open Util.Assert
open X86
open Simulator
open Gradedtests

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* NOTE: Your "submitted public test case" for Part III belongs over in the
   shared git submodule.
*)


let mov_ri =
 test_machine
 [
 InsB0 (Movq, Asm.[ ~$42; ~%Rax ]);
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 InsFrag;
 ]


let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  ]);

] 
