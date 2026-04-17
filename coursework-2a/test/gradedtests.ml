open Oat
open Ast
open Astlib
open Util.Assert
open Driver

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your project.                   *)

(* These tests will be used to grade your assignment *)

(* Compares two values according to [f], printing an error message in case of disagreement. *)
let assert_eq_ast (f: 'a -> 'a -> bool) (print : 'a -> string) (x: 'a) (y: unit -> 'a)  : assertion =
  fun () ->
  let result = y () in
  if f x result then () else
      let msg = Printf.sprintf "\nEXP: %s\nGOT: %s\n" (print x) (print result) in
      failwith msg

(* General purpose test for a parsing function. Parses the code [string] and compares it for
   equality with the proved [ast].
 *)
let parse_test parse (compare : 'a -> 'a -> bool) (printer : 'a -> string) (code : string) (ast : 'a)  : assertion =
  let lexbuf = Lexing.from_string code in
  try 
    assert_eq_ast compare printer ast (fun () -> (parse Lexer.token lexbuf))
  with
  | Parser.Error -> failwith @@ Printf.sprintf "Parse error at: %s"
      (Range.string_of_range (Range.lex_range lexbuf))
  | Lexer.Lexer_error (r, s) -> failwith @@ Printf.sprintf "Lexer error at: %s %s"
                                              (Range.string_of_range r) s
  | _ -> failwith @@ Printf.sprintf "Unknown Parse error at %s"
                       (Range.string_of_range (Range.lex_range lexbuf))

(* Parse test for expressions. Note that [Astlib.eq_exp] compares abstract syntax trees
   up to equality *ignoring* the location range data decorting the nodes.

   Note: it can sometimes be useful to replace [string_of_exp] (the normal pretty printer)
   with [ml_string_of_exp], which prints an OCaml representation of the ast.
 *)
let exp_test code ast = parse_test Parser.exp_top eq_exp string_of_exp code ast

(* Sabe as [exp_test} but parses a [stmt] component of the grammar. *)
let stmt_test code ast = parse_test Parser.stmt_top eq_stmt string_of_stmt code ast

let parse_consts =
  [ ("parse consts test one", exp_test "bool[] null" (no_loc (CNull (RArray TBool))))
  ; ("parse consts test two", exp_test "42" (no_loc (CInt 42L)))
  ; ("parse consts test three", exp_test "true" (no_loc (CBool true)))
  ; ("parse consts test four", exp_test "false" (no_loc (CBool false)))
  ; ("parse consts test five", exp_test "\"hello world\"" (no_loc (CStr "hello world")))
  ; ("parse consts test six", exp_test "new int[]{1, 2, 3}" (no_loc (CArr (TInt, [no_loc (CInt 1L); no_loc (CInt 2L); no_loc (CInt 3L)]))))
  ]

let mk_id x = no_loc (Lhs (no_loc (Id x)))
let mk_Index (x,y) = no_loc (Lhs (no_loc (Index (x,y))))

(* Hand written expression tests *)
let parse_exp_tests =
  [ ("parse exp test 1", exp_test "1" (no_loc (CInt 1L)))
  ; ("parse exp test 2", exp_test "1+2" (no_loc (Bop (Add,no_loc (CInt 1L),no_loc (CInt 2L)))))
  ; ("parse exp test 3", exp_test "1+2+3" (no_loc (Bop (Add,no_loc (Bop (Add,no_loc (CInt 1L),no_loc (CInt 2L))),no_loc (CInt 3L)))))
  ; ("parse exp test 4", exp_test "1+2*3" (no_loc (Bop (Add,no_loc (CInt 1L),no_loc (Bop (Mul,no_loc (CInt 2L),no_loc (CInt 3L)))))))
  ; ("parse exp test 5", exp_test "1+(2+3)" (no_loc (Bop (Add,no_loc (CInt 1L),no_loc (Bop (Add,no_loc (CInt 2L),no_loc (CInt 3L)))))))
  ; ("parse exp test 6", exp_test "(1+2)*3" (no_loc (Bop (Mul,no_loc (Bop (Add,no_loc (CInt 1L),no_loc (CInt 2L))),no_loc (CInt 3L)))))
  ; ("parse exp test 7", exp_test "1+2*3+4" (no_loc (Bop (Add,no_loc (Bop (Add,no_loc (CInt 1L),no_loc (Bop (Mul,no_loc (CInt 2L),no_loc (CInt 3L))))),no_loc (CInt 4L)))))
  ; ("parse exp test 8", exp_test "1-2 == 3+4" (no_loc (Bop (Eq,no_loc (Bop (Sub,no_loc (CInt 1L),no_loc (CInt 2L))),no_loc (Bop (Add,no_loc (CInt 3L),no_loc (CInt 4L)))))))
  ; ("parse exp test 9", exp_test "(1+2)*(3+4)" (no_loc (Bop (Mul,no_loc (Bop (Add,no_loc (CInt 1L),no_loc (CInt 2L))),no_loc (Bop (Add,no_loc (CInt 3L),no_loc (CInt 4L)))))))
  ; ("parse exp test 10", exp_test "true & true | false" (no_loc (Bop (Or,no_loc (Bop (And,no_loc (CBool true),no_loc (CBool true))),no_loc (CBool false)))))
  ; ("parse exp test 11", exp_test "true & (true | false)" (no_loc (Bop (And,no_loc (CBool true),no_loc (Bop (Or,no_loc (CBool true),no_loc (CBool false)))))))
  ; ("parse exp test 12", exp_test "!(~5 == ~6) & -5+10 < 0" (no_loc (Bop (And,no_loc (Uop (Lognot, no_loc (Bop (Eq,no_loc (Uop (Bitnot, no_loc (CInt 5L))),no_loc (Uop (Bitnot, no_loc (CInt 6L))))))),no_loc (Bop (Lt,no_loc (Bop (Add,no_loc (Uop (Neg, no_loc (CInt 5L))),no_loc (CInt 10L))),no_loc (CInt 0L)))))))
  ; ("parse exp test 13", exp_test "1+2 >> (3-4 >>> 7*8) << 9" (no_loc (Bop (Shl,no_loc (Bop (Shr,no_loc (Bop (Add,no_loc (CInt 1L),no_loc (CInt 2L))),no_loc (Bop (Sar,no_loc (Bop (Sub,no_loc (CInt 3L),no_loc (CInt 4L))),no_loc (Bop (Mul,no_loc (CInt 7L),no_loc (CInt 8L))))))),no_loc (CInt 9L)))))
  ; ("parse exp test 14", exp_test "~5 >> 7 - 10 < 9 * -6-4 | !false" (no_loc (Bop (Or,no_loc (Bop (Lt,no_loc (Bop (Shr,no_loc (Uop (Bitnot, no_loc (CInt 5L))),no_loc (Bop (Sub,no_loc (CInt 7L),no_loc (CInt 10L))))),no_loc (Bop (Sub,no_loc (Bop (Mul,no_loc (CInt 9L),no_loc (Uop (Neg, no_loc (CInt 6L))))),no_loc (CInt 4L))))),no_loc (Uop (Lognot, no_loc (CBool false)))))))
  ; ("parse exp test 15", exp_test "false == 2 >= 3 | true !=  9 - 10 <= 4" (no_loc (Bop (Or,no_loc (Bop (Eq,no_loc (CBool false),no_loc (Bop (Gte,no_loc (CInt 2L),no_loc (CInt 3L))))),no_loc (Bop (Neq,no_loc (CBool true),no_loc (Bop (Lte,no_loc (Bop (Sub,no_loc (CInt 9L),no_loc (CInt 10L))),no_loc (CInt 4L)))))))))
  ; ("parse exp test 16", exp_test "1-2*3+4 < 5 | 6+7-2 > 1 | true & false" (no_loc (Bop (Or,no_loc (Bop (Or,no_loc (Bop (Lt,no_loc (Bop (Add,no_loc (Bop (Sub,no_loc (CInt 1L),no_loc (Bop (Mul,no_loc (CInt 2L),no_loc (CInt 3L))))),no_loc (CInt 4L))),no_loc (CInt 5L))),no_loc (Bop (Gt,no_loc (Bop (Sub,no_loc (Bop (Add,no_loc (CInt 6L),no_loc (CInt 7L))),no_loc (CInt 2L))),no_loc (CInt 1L))))),no_loc (Bop (And,no_loc (CBool true),no_loc (CBool false)))))))
  ; ("parse exp test 17", exp_test "true [&] false | false [|] true & true" (no_loc (Bop (IOr,no_loc (Bop (IAnd,no_loc (CBool true),no_loc (Bop (Or,no_loc (CBool false),no_loc (CBool false))))),no_loc (Bop (And,no_loc (CBool true),no_loc (CBool true)))))))
  ; ("parse exp test 18", exp_test "true [|] false [&] true & true | false" (no_loc (Bop (IOr,no_loc (CBool true),no_loc (Bop (IAnd,no_loc (CBool false),no_loc (Bop (Or,no_loc (Bop (And,no_loc (CBool true),no_loc (CBool true))),no_loc (CBool false)))))))))
  ; ("parse exp test 19", exp_test "new int[3]" (no_loc (NewArr (TInt,no_loc (CInt 3L)))))
  ; ("parse exp test 20", exp_test "bar (x, \"cis341\")" (no_loc (Call ("bar", [ mk_id "x" ; no_loc (CStr "cis341") ]))))
  ; ("parse exp test 21", exp_test "new int[3]" (no_loc (NewArr (TInt,no_loc (CInt 3L)))))
  ; ("parse exp test 22", exp_test "new int[][]{new int[]{10,11},new int[]{20,21},new int[]{30,31}}" (no_loc (CArr (TRef (RArray TInt), [ no_loc (CArr (TInt, [ no_loc (CInt 10L) ; no_loc (CInt 11L) ])) ; no_loc (CArr (TInt, [ no_loc (CInt 20L) ; no_loc (CInt 21L) ])) ; no_loc (CArr (TInt, [ no_loc (CInt 30L) ; no_loc (CInt 31L) ])) ]))))
  ; ("parse exp test 23", exp_test "proc1 ()" (no_loc (Call ("proc1", [  ]))))
  ; ("parse exp test 24", exp_test "array[0]" (no_loc (Lhs (no_loc (Index (mk_id "array", no_loc (CInt 0L)))))))
  ; ("parse exp test 25", exp_test "i + y[1][1]" (no_loc (Bop (Add,mk_id "i", no_loc(Lhs (no_loc (Index (no_loc (Lhs (no_loc (Index (mk_id "y", no_loc (CInt 1L))))), no_loc (CInt 1L)))))))))
  ; ("parse exp test 26", exp_test "-!~x[0][0]" (no_loc (Uop (Neg, no_loc (Uop (Lognot, no_loc (Uop (Bitnot, no_loc (Lhs (no_loc (Index (no_loc (Lhs (no_loc (Index (mk_id "x", no_loc (CInt 0L))))), no_loc (CInt 0L)))))))))))))
  ; ("parse exp test 27", exp_test "print_string (string_concat (str1, str2))" (no_loc (Call ("print_string", [ no_loc (Call ("string_concat", [ mk_id "str1" ; mk_id "str2" ])) ]))))
  ]

let parse_stmt_tests = 
  [ ("parse stmt test 1", stmt_test "var n = 8;" Progasts.stmt_test_1) 
  ; ("parse stmt test 2", stmt_test "var x=a[0];" Progasts.stmt_test_2)
  ; ("parse stmt test 3", stmt_test "return;" Progasts.stmt_test_3)
  ; ("parse stmt test 4", stmt_test "return x+y;" Progasts.stmt_test_4)
  ; ("parse stmt test 5", stmt_test "a[j>>1]=v;" Progasts.stmt_test_5)
  ; ("parse stmt test 6", stmt_test "foo(a,1,n);" Progasts.stmt_test_6)
  ; ("parse stmt test 7", stmt_test "a[i]=a[i>>1];" Progasts.stmt_test_7)
  ; ("parse stmt test 8", stmt_test "var a = new int[8];" Progasts.stmt_test_8)
  ; ("parse stmt test 9", stmt_test "if((j<n)&(a[j]<a[j+1])) { j=j+1; }" Progasts.stmt_test_9)
  ; ("parse stmt test 10", stmt_test "if (c == 1) { var i = 0; var j = 0; var k = 0; }" Progasts.stmt_test_10)
  ; ("parse stmt test 11", stmt_test "while((i>1)&(a[i>>1]<v)) { a[i]=a[i>>1]; i=i>>1; }" Progasts.stmt_test_11)
  ; ("parse stmt test 12", stmt_test "for (; i > 0; i=i-1;) { for (var j = 1; j <= i; j=j+1;) { if (numbers[j-1] > numbers[i]) { temp = numbers[j-1]; numbers[j-1] = numbers[i]; numbers[i] = temp; } } }" Progasts.stmt_test_12)
  ; ("parse stmt test 13", stmt_test "for (var i = 0, var j = 0; ;) { }" Progasts.stmt_test_13)
  ]

(* tests whether a given file parses as the given ast *)
let parse_file_test filepath ast =
  assert_eq_ast Astlib.eq_prog ml_string_of_prog ast (fun () -> Driver.parse_oat_file filepath)

let parse_prog_tests =
  [ ("parse prog test 1", parse_file_test "cw2aprograms/easy_p1.oat" Progasts.easy_p1_ast)
  ; ("parse prog test 2", parse_file_test "cw2aprograms/easy_p2.oat" Progasts.easy_p2_ast)
  ; ("parse prog test 3", parse_file_test "cw2aprograms/easy_p3.oat" Progasts.easy_p3_ast)
  ; ("parse prog test 4", parse_file_test "cw2aprograms/easy_p4.oat" Progasts.easy_p4_ast)
  ; ("parse prog test 5", parse_file_test "cw2aprograms/easy_p5.oat" Progasts.easy_p5_ast)
  ; ("parse prog test 6", parse_file_test "cw2aprograms/easy_p6.oat" Progasts.easy_p6_ast)
  ; ("parse prog test 7", parse_file_test "cw2aprograms/easy_p7.oat" Progasts.easy_p7_ast)
  ]

  
let parse_tests = parse_consts
                @ parse_exp_tests
                @ parse_stmt_tests
                @ parse_prog_tests


let oat_file_test path args =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in

  let output_path = !Platform.output_path in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in

  let oat_ast = parse_oat_file path in
  let ll_ast = Frontend.cmp_prog oat_ast in
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let () = write_file dot_ll_file ll_str in
  let () = Platform.link (dot_ll_file::["bin/runtime.c"]) exec_file in

  let result = Driver.run_program_error args exec_file tmp_file in
  (*  let () = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file) Platform.ignore_error in *)
  let () = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let executed_oat_file tests =
  List.map (fun (path, args, ans) ->
      (path ^ " args: " ^ args), assert_eqfs (fun () -> oat_file_test path args) ans)
    tests

let easiest_tests = [
  ("cw2aprograms/easyrun1.oat", "", "17");
  ("cw2aprograms/easyrun2.oat", "", "35");
  ("cw2aprograms/easyrun3.oat", "", "73");
  ("cw2aprograms/easyrun4.oat", "", "6");
  ("cw2aprograms/easyrun5.oat", "", "212");
  ("cw2aprograms/easyrun6.oat", "", "9");
  ("cw2aprograms/easyrun7.oat", "", "23");
  ("cw2aprograms/easyrun8.oat", "", "160");
  ("cw2aprograms/easyrun9.oat", "", "236");
  ("cw2aprograms/easyrun10.oat", "", "254");
]

let globals_tests = [
  ("cw2aprograms/globals1.oat", "", "42");
  ("cw2aprograms/globals2.oat", "", "17");
  ("cw2aprograms/globals3.oat", "", "17");
  ("cw2aprograms/globals4.oat", "", "5");
  ("cw2aprograms/globals5.oat", "", "17");
  ("cw2aprograms/globals6.oat", "", "15");
  ("cw2aprograms/globals7.oat", "", "3");
]

let path_tests = [
 ("cw2aprograms/path1.oat", "", "17");
 ("cw2aprograms/path2.oat", "", "35");
 ("cw2aprograms/path3.oat", "", "3");
 ("cw2aprograms/arrayargs.oat", "", "17");
 ("cw2aprograms/arrayargs1.oat", "", "17");
 ("cw2aprograms/arrayargs2.oat", "", "17");
 ("cw2aprograms/arrayargs3.oat", "", "34");
]

let easy_tests = [
    ("cw2aprograms/argassign.oat", "", "18");
    ("cw2aprograms/run13.oat", "", "1");
    ("cw2aprograms/run21.oat", "", "99");
    ("cw2aprograms/run26.oat", "", "0");
    ("cw2aprograms/run27.oat", "", "99");
    ("cw2aprograms/run28.oat", "", "18");
    ("cw2aprograms/run29.oat", "", "1");
    ("cw2aprograms/run30.oat", "", "9");
    ("cw2aprograms/run31.oat", "", "9");
    ("cw2aprograms/run32.oat", "", "33");
    ("cw2aprograms/run33.oat", "", "1");
    ("cw2aprograms/run34.oat", "", "66");
    ("cw2aprograms/run35.oat", "", "66");
    ("cw2aprograms/run36.oat", "", "0");
    ("cw2aprograms/run37.oat", "", "2");
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

let medium_tests = [
  ("cw2aprograms/fact.oat", "", "factorial(5) =1200");
  ("cw2aprograms/run1.oat", "", "153");
  ("cw2aprograms/run2.oat", "", "6");
  ("cw2aprograms/run3.oat", "", "2");
  ("cw2aprograms/run4.oat", "", "42");
  ("cw2aprograms/run5.oat", "", "4");
  ("cw2aprograms/run6.oat", "", "1");
  ("cw2aprograms/run7.oat", "", "20");
  ("cw2aprograms/run8.oat", "", "2");
  ("cw2aprograms/run9.oat", "", "4");
  ("cw2aprograms/run10.oat", "", "5");
  ("cw2aprograms/run11.oat", "", "7");
  ("cw2aprograms/run14.oat", "", "16");
  ("cw2aprograms/run15.oat", "", "19");
  ("cw2aprograms/run16.oat", "", "13");
  ("cw2aprograms/run18.oat", "", "231");
  ("cw2aprograms/run19.oat", "", "231");
  ("cw2aprograms/run20.oat", "", "19");
  ("cw2aprograms/run22.oat", "", "abc0");
  ("cw2aprograms/run23.oat", "", "1230");
  ("cw2aprograms/run24.oat", "", "0");
  ("cw2aprograms/run25.oat", "", "nnn0");
  ("cw2aprograms/run43.oat", "", "42");
  ("cw2aprograms/run44.oat", "", "hello0");
  ("cw2aprograms/run45.oat", "", "420");
  ("cw2aprograms/run46.oat", "", "420");
  ("cw2aprograms/run47.oat", "", "3");
  ("cw2aprograms/run48.oat", "", "11");
  ("cw2aprograms/run53.oat", "", "nnn0");
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

let hard_tests = [
("cw2aprograms/fac.oat", "", "120");
("cw2aprograms/qsort.oat", "", "kpyf{shomfhkmopsy{255");
("cw2aprograms/bsort.oat", "", "y}xotnuw notuwxy}255");
("cw2aprograms/msort.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("cw2aprograms/msort2.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("cw2aprograms/selectionsort.oat", "", "01253065992000");
("cw2aprograms/matrixmult.oat", "", "19 16 13 23 \t5 6 7 6 \t19 16 13 23 \t5 6 7 6 \t0");
]

let old_student_tests = [
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
  ; ("cw2aprograms/life.oat", "", "00101001100101000")
  ; ("cw2aprograms/lcs.oat", "", "OAT0")
  ; ("cw2aprograms/insertion_sort.oat", "", "42")
  ; ("cw2aprograms/maxsubsequence.oat", "", "107")
]


let tests : suite =
  List.map (Util.Assert.timeout_test 10)
  [ GradedTest("parse tests", 15, parse_tests);
    GradedTest("easiest tests", 15, executed_oat_file easiest_tests);
    GradedTest("globals tests", 15, executed_oat_file globals_tests);
    GradedTest("path tests", 10, executed_oat_file path_tests);
    GradedTest("easy tests", 10, executed_oat_file easy_tests);
    GradedTest("medium tests", 10, executed_oat_file medium_tests);
    GradedTest("hard tests", 10, executed_oat_file (hard_tests @ old_student_tests));
    GradedTest("hidden tests", 5, [] )
  ]


let graded_tests : suite = tests
