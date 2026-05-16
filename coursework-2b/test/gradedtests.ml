open Testlib
open Util.Assert
open X86
open Ll
module Driver = Oat.Driver
module Backend = Oat.Backend
module Typechecker = Oat.Typechecker
module Frontend = Oat.Frontend
module Tctxt = Oat.Tctxt
open Backend
open Driver

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your project.                   *)

(* These tests will be used to grade your assignment *)


let unit_tests = [
  "subtype_stringQ_stringQ",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TNullRef RString) (TNullRef RString) then ()
       else failwith "should not fail")
; ("no_subtype_stringQ_stringQ",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TNullRef RString) (TRef RString) then
         failwith "should not succeed" else ())
  )
]


let cw2a_easiest_tests = [
  ("cw2aprograms/easyrun1.oat", "", "17");
  ("cw2aprograms/easyrun2.oat", "", "35");
  ("cw2aprograms/easyrun3.oat", "", "73");
  ("cw2aprograms/easyrun4.oat", "", "6");
  ("cw2aprograms/easyrun5.oat", "", "212");
  ("cw2aprograms/easyrun6.oat", "", "9");
  ("cw2aprograms/easyrun7.oat", "", "23");
  ("cw2aprograms/easyrun8.oat", "", "160");
  ("cw2aprograms/easyrun9.oat", "", "236");
]

let cw2a_globals_tests = [
  ("cw2aprograms/globals1.oat", "", "42");
  ("cw2aprograms/globals2.oat", "", "17");
  ("cw2aprograms/globals3.oat", "", "17");
  ("cw2aprograms/globals4.oat", "", "5");
  ("cw2aprograms/globals5.oat", "", "17");
  ("cw2aprograms/globals6.oat", "", "15");
  ("cw2aprograms/globals7.oat", "", "3");
]

let cw2a_path_tests = [
 ("cw2aprograms/path1.oat", "", "17");
 ("cw2aprograms/path2.oat", "", "35");
 ("cw2aprograms/path3.oat", "", "3");
 ("cw2aprograms/arrayargs1.oat", "", "17");
 ("cw2aprograms/arrayargs2.oat", "", "17");
 ("cw2aprograms/arrayargs3.oat", "", "34");
]

let cw2a_easy_tests = [
    ("cw2aprograms/run26.oat", "", "0");
    ("cw2aprograms/run27.oat", "", "99");
    ("cw2aprograms/run28.oat", "", "18");
    ("cw2aprograms/run29.oat", "", "1");
    ("cw2aprograms/run30.oat", "", "9");
    ("cw2aprograms/run31.oat", "", "9");
    ("cw2aprograms/run13.oat", "", "1");
    ("cw2aprograms/run32.oat", "", "33");
    ("cw2aprograms/run21.oat", "", "99");
    ("cw2aprograms/run33.oat", "", "1");
    ("cw2aprograms/run34.oat", "", "66");
    ("cw2aprograms/run38.oat", "", "31");
    ("cw2aprograms/run39.oat", "a", "2");
    ("cw2aprograms/run40.oat", "", "8");
    ("cw2aprograms/run41.oat", "", "3");
    ("cw2aprograms/run42.oat", "", "2");
    ("cw2aprograms/run49.oat", "", "abc0");
    ("cw2aprograms/run50.oat", "", "abcde0");
    ("cw2aprograms/run60.oat", "", "85");
    ("cw2aprograms/run61.oat", "", "3410");
]

let cw2a_medium_tests = [
  ("cw2aprograms/fact.oat", "", "factorial(5) =1200");
  ("cw2aprograms/run1.oat", "", "153");
  ("cw2aprograms/run2.oat", "", "6");
  ("cw2aprograms/run8.oat", "", "2");
  ("cw2aprograms/run9.oat", "", "4");
  ("cw2aprograms/run10.oat", "", "5");
  ("cw2aprograms/run11.oat", "", "7");
  ("cw2aprograms/run14.oat", "", "16");
  ("cw2aprograms/run15.oat", "", "19");
  ("cw2aprograms/run16.oat", "", "13");
  ("cw2aprograms/run22.oat", "", "abc0");
  ("cw2aprograms/run23.oat", "", "1230");
  ("cw2aprograms/run25.oat", "", "nnn0");
  ("cw2aprograms/run46.oat", "", "420");
  ("cw2aprograms/run47.oat", "", "3");
  ("cw2aprograms/run48.oat", "", "11");
  ("cw2aprograms/lib4.oat", "", "53220");
  ("cw2aprograms/lib5.oat", "", "20");
  ("cw2aprograms/lib6.oat", "", "56553");
  ("cw2aprograms/lib7.oat", "", "53");
  ("cw2aprograms/lib8.oat", "", "Hello world!0");
  ("cw2aprograms/lib9.oat", "a b c d", "abcd5");
  ("cw2aprograms/lib11.oat", "", "45");
  ("cw2aprograms/lib14.oat", "", "~}|{zyxwvu0");
  ("cw2aprograms/lib15.oat", "123456789", "456780");
]

let cw2a_hard_tests = [
("cw2aprograms/fac.oat", "", "120");
("cw2aprograms/qsort.oat", "", "kpyf{shomfhkmopsy{255");
("cw2aprograms/bsort.oat", "", "y}xotnuw notuwxy}255");
("cw2aprograms/msort.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("cw2aprograms/msort2.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("cw2aprograms/selectionsort.oat", "", "01253065992000");
("cw2aprograms/matrixmult.oat", "", "19 16 13 23 \t5 6 7 6 \t19 16 13 23 \t5 6 7 6 \t0");
]

let cw2a_old_student_tests = [
    ("cw2aprograms/binary_search.oat", "", "Correct!0")
  ; ("cw2aprograms/xor_shift.oat", "", "838867572\n22817190600")
  ; ("cw2aprograms/sieve.oat", "", "25")
  ; ("cw2aprograms/count_sort.oat", "", "AFHZAAEYC\nAAACEFHYZ0")
  ; ("cw2aprograms/fibo.oat", "", "0")
  ; ("cw2aprograms/heap.oat", "", "1")
  ; ("cw2aprograms/binary_gcd.oat", "", "3")
  ; ("cw2aprograms/lfsr.oat", "", "TFTF FFTT0")
  ; ("cw2aprograms/gnomesort.oat", "", "01253065992000")
  ; ("cw2aprograms/josh_joyce_test.oat", "", "0")
  ; ("cw2aprograms/gcd.oat", "", "16")
  ; ("cw2aprograms/lcs.oat", "", "OAT0")
  ; ("cw2aprograms/insertion_sort.oat", "", "42")
  ; ("cw2aprograms/maxsubsequence.oat", "", "107")
]

let cw2a_type_error_tests = [
  "cw2aprograms/run3.oat"
; "cw2aprograms/run5.oat"
; "cw2aprograms/run35.oat"
; "cw2aprograms/run43.oat"
; "cw2aprograms/run44.oat"
; "cw2aprograms/run45.oat"
]


let typecheck_equality_tests = [
    "cw2bprograms/tc_eq1.oat"
  ; "cw2bprograms/tc_eq2.oat"
  ]

let struct_tests = [
("cw2bprograms/compile_assign_struct.oat", "", "16");
("cw2bprograms/compile_basic_struct.oat", "", "7");
("cw2bprograms/compile_global_struct.oat", "", "254");
("cw2bprograms/compile_nested_struct.oat", "", "10");
("cw2bprograms/compile_return_struct.oat", "", "0");
("cw2bprograms/compile_struct_array.oat", "", "15");
("cw2bprograms/compile_struct_fptr.oat", "", "7");
("cw2bprograms/compile_various_fields.oat", "", "hello253");
]

let fptr_tests = [
  ("cw2bprograms/compile_array_fptr.oat", "", "2");
  ("cw2bprograms/compile_func_argument.oat", "", "4");
  ("cw2bprograms/compile_scall_fptr.oat", "", "4");
  ("cw2bprograms/compile_var_fptr.oat", "", "1");
  ("cw2bprograms/compile_local_fptr.oat", "", "5");
  ("cw2bprograms/compile_function_shadow.oat", "", "12");
  ("cw2bprograms/compile_builtin_argument.oat", "", "abab0");
]

let typecheck_subtyping_tests =
  [ "cw2bprograms/tc_subtyping1.oat"
  ; "cw2bprograms/tc_subtyping2.oat"
  ; "cw2bprograms/tc_subtyping3.oat"
  ; "cw2bprograms/tc_subtyping4.oat"
  ; "cw2bprograms/tc_subtyping5.oat"
  ; "cw2bprograms/tc_subtyping6.oat"
  ; "cw2bprograms/tc_subtyping7.oat"
  ; "cw2bprograms/tc_subtyping8.oat"
  ; "cw2bprograms/tc_subtyping9.oat"
  ]

let typecheck_subtyping_error_tests =
  [ "cw2bprograms/tc_subtyping_err1.oat"
  ; "cw2bprograms/tc_subtyping_err2.oat"
  ; "cw2bprograms/tc_subtyping_err3.oat"
  ; "cw2bprograms/tc_subtyping_err4.oat"
  ; "cw2bprograms/tc_subtyping_err5.oat"
  ; "cw2bprograms/tc_subtyping_err6.oat"
  ; "cw2bprograms/tc_subtyping_err7.oat"
  ; "cw2bprograms/tc_subtyping_err8.oat"
  ]


let typecheck_statement_error_tests =
  [ "cw2bprograms/tc_error_early_return.oat";
    "cw2bprograms/tc_error_early_return_void.oat";
    "cw2bprograms/tc_error_return_wrong.oat";
    "cw2bprograms/tc_error_while_nonbool.oat";
    "cw2bprograms/tc_error_while.oat";
    "cw2bprograms/tc_error_if_nonbool.oat";
    "cw2bprograms/tc_error_if.oat";
    "cw2bprograms/tc_error_for.oat";
    "cw2bprograms/tc_error_void.oat";
    "cw2bprograms/tc_error_assign_void.oat";
    "cw2bprograms/tc_error_scall_nonvoid.oat";
  ]

let typecheck_correct_statement_tests =
  [ "cw2bprograms/tc_correct_while.oat";
    "cw2bprograms/tc_correct_for.oat";
    "cw2bprograms/tc_correct_if.oat";
    "cw2bprograms/tc_correct_void.oat"
  ]

let typecheck_error_expression_tests =
  [ "cw2bprograms/tc_error_binop1.oat";
    "cw2bprograms/tc_error_binop2.oat";
    "cw2bprograms/tc_error_binop3.oat";
    "cw2bprograms/tc_error_call1.oat";
    "cw2bprograms/tc_error_call2.oat";
    "cw2bprograms/tc_error_unop1.oat";
    "cw2bprograms/tc_error_array1.oat";
    "cw2bprograms/tc_error_array2.oat";
    "cw2bprograms/tc_error_array3.oat";
    "cw2bprograms/tc_error_array4.oat";
    "cw2bprograms/tc_error_null.oat";
  ]

let typecheck_error_struct_tests =
  [ "cw2bprograms/tc_error_struct_proj.oat";
    "cw2bprograms/tc_error_struct1.oat";
    "cw2bprograms/tc_error_struct2.oat";
    "cw2bprograms/tc_error_struct3.oat";
    "cw2bprograms/tc_error_struct4.oat";
    "cw2bprograms/tc_error_struct_dup.oat";
    "cw2bprograms/tc_error_struct.oat";
    "cw2bprograms/tc_error_dupstruct.oat";
    "cw2bprograms/tc_error_struct_unbound.oat";
  ]

let typecheck_error_global_tests =
  [ "cw2bprograms/tc_error_global_dup.oat";
    "cw2bprograms/tc_error_global.oat";
    "cw2bprograms/tc_error_func_redeclaration.oat";
    "cw2bprograms/tc_error_func_assign.oat";
    "cw2bprograms/tc_error_overwrite.oat";
    "cw2bprograms/tc_error_function_no_shadow.oat";
    "cw2bprograms/tc_correct_null.oat";
  ]

let typecheck_correct_other_tests =
  [ "cw2bprograms/tc_correct_array.oat";
    "cw2bprograms/tc_correct_array2.oat";
    "cw2bprograms/tc_correct_array3.oat";
    "cw2bprograms/tc_correct_call.oat";
    "cw2bprograms/tc_correct_fptr.oat";
    "cw2bprograms/tc_correct_global.oat";
    "cw2bprograms/tc_correct_struct.oat";
    "cw2bprograms/tc_correct_struct_fptr.oat";
    "cw2bprograms/tc_correct_void.oat";
    "cw2bprograms/tc_correct_local_redeclaration.oat";
    "cw2bprograms/tc_correct_fptr_array.oat";
    "cw2bprograms/tc_struct_null_field.oat";
  ]

let typecheck_error_null_not_null_tests =
  cw2a_type_error_tests


let fptr_tests = [
  ("cw2bprograms/compile_array_fptr.oat", "", "2");
  ("cw2bprograms/compile_func_argument.oat", "", "4");
  ("cw2bprograms/compile_scall_fptr.oat", "", "4");
  ("cw2bprograms/compile_var_fptr.oat", "", "1");
  ("cw2bprograms/compile_local_fptr.oat", "", "5");
  ("cw2bprograms/compile_function_shadow.oat", "", "12");
  ("cw2bprograms/compile_builtin_argument.oat", "", "abab0");
]


let new_tests = [
  ("cw2bprograms/ifq1.oat", "", "4");
  ("cw2bprograms/ifq2.oat", "", "5");
  ("cw2bprograms/ifq3.oat", "", "6");
  ("cw2bprograms/ifq4.oat", "", "4");
  ("cw2bprograms/ifq5.oat", "", "4");
  ("cw2bprograms/run44fixed.oat", "", "hello0");
  ("cw2bprograms/length1.oat", "", "5");
  ("cw2bprograms/compile_array_init.oat", "", "2");
  ("cw2bprograms/array_oob.oat", "", "Out of bounds index 3 for array length 30001");
  ("cw2bprograms/conquest.oat", "", "My name is Jeff...\nCharizard is the BEST Pokemon ever!!!11");
]

let tc_ok_tests = [
  "cw2bprograms/tc_struct_ok.oat"
; "cw2bprograms/tc_func_ret_ok.oat"
; "cw2bprograms/tc_func_arg_ok.oat"
; "cw2bprograms/tc_ifq1.oat"
; "cw2aprograms/tc_ok1.oat"
; "cw2aprograms/tc_ok2.oat"
; "cw2aprograms/tc_ok4.oat"
; "cw2aprograms/tc_ok5.oat"
; "cw2aprograms/tc_ok6.oat"
; "cw2aprograms/tc_ok7.oat"
; "cw2aprograms/tc_ok8.oat"
; "cw2bprograms/tc_arrow.oat"
; "cw2bprograms/tc_arrow_null.oat"
; "cw2bprograms/tc_arrow_null_rec.oat"
]

let tc_err_tests = [
  "cw2bprograms/tc_null_array_err.oat"
; "cw2bprograms/tc_struct_err.oat"
; "cw2bprograms/tc_func_ret_err.oat"
; "cw2bprograms/tc_func_arg_err.oat"
; "cw2bprograms/tc_array_err.oat"
; "cw2bprograms/tc_struct_field_err.oat"
; "cw2bprograms/tc_recursive_struct_err.oat"
; "cw2bprograms/tc_ifq_err1.oat"
]


let typecheck_tests : suite = [
  GradedTest("subtype unit tests", 1, unit_tests);
  GradedTest("tc subtyping tests", 4, typecheck_file_correct typecheck_subtyping_tests);
  GradedTest("tc subtyping error tests", 4, typecheck_file_error typecheck_subtyping_error_tests);
  GradedTest("tc equality tests", 4, typecheck_file_correct typecheck_equality_tests);
  GradedTest("tc statement error tests", 4, typecheck_file_error typecheck_statement_error_tests);
  GradedTest("tc statement correct tests", 1, typecheck_file_correct typecheck_correct_statement_tests);
  GradedTest("tc other correct tests", 4, typecheck_file_correct typecheck_correct_other_tests);
  GradedTest("tc null/not null error tests", 2, typecheck_file_error typecheck_error_null_not_null_tests);
  GradedTest("tc expression error tests", 4, typecheck_file_error typecheck_error_expression_tests);
  GradedTest("tc struct/global error tests", 5, typecheck_file_error (typecheck_error_struct_tests @ typecheck_error_global_tests));
  GradedTest("extra tc err tests", 5, typecheck_file_error tc_err_tests)
]

let cw2b_tests : suite = [
  GradedTest("tc ok tests", 8, executed_tc_ok_file tc_ok_tests)
; GradedTest("new tests", 8, executed_oat_file new_tests)
; GradedTest("struct tests", 8, executed_oat_file struct_tests)
; GradedTest("fptr tests", 2, executed_oat_file fptr_tests)
  ]








let complex_tests : suite =
  [GradedTest ("complex hidden tests", 6, executed_oat_file 
  [] )]

let hidden_unit_tests : suite = 
  [ GradedTest ("hidden typehecking unit tests", 10,
[])
  ]

let manual_tests : suite = [
  GradedTest ("Public Test Cases", 5,
              []
             )
] 

let cw2a_tests =
      cw2a_easiest_tests
    @ cw2a_globals_tests
    @ cw2a_path_tests
    @ cw2a_easy_tests
    @ cw2a_medium_tests
    @ cw2a_hard_tests
    @ cw2a_old_student_tests

let functionality_tests : suite = [GradedTest("functionality tests from CW2a", 10, executed_oat_file cw2a_tests)]

let graded_tests : suite =
  typecheck_tests @
  hidden_unit_tests @
  cw2b_tests @
  complex_tests @
  functionality_tests @
  manual_tests
