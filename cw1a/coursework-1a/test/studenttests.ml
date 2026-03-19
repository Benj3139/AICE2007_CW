open Util.Assert
open X86
open Simulator
open Gradedtests

let mk_insertion_sort_prog (a0 : int64) (a1 : int64) (a2 : int64) (a3 : int64) (a4 : int64) : prog =
  [
    { lbl = "main"; global = true; asm = Text [
        (* Allocate 5 qwords on the stack and initialize array *)
        (Subq, [Imm (Lit 40L); Reg Rsp]);
        (Movq, [Imm (Lit a0); Ind3 (Lit 0L, Rsp)]);
        (Movq, [Imm (Lit a1); Ind3 (Lit 8L, Rsp)]);
        (Movq, [Imm (Lit a2); Ind3 (Lit 16L, Rsp)]);
        (Movq, [Imm (Lit a3); Ind3 (Lit 24L, Rsp)]);
        (Movq, [Imm (Lit a4); Ind3 (Lit 32L, Rsp)]);

        (* i = 1 *)
        (Movq, [Imm (Lit 1L); Reg Rcx]);
        (Jmp, [Imm (Lbl "main_loop")]);
      ]};

    { lbl = "main_loop"; global = false; asm = Text [
        (* for i in 1..4 *)
        (Cmpq, [Imm (Lit 5L); Reg Rcx]);
        (J Ge, [Imm (Lbl "verify")]);

        (* key = a[i] *)
        (Movq, [Reg Rcx; Reg R08]);
        (Shlq, [Imm (Lit 3L); Reg R08]);
        (Addq, [Reg Rsp; Reg R08]);
        (Movq, [Ind2 R08; Reg Rax]);

        (* j = i - 1 *)
        (Movq, [Reg Rcx; Reg Rdx]);
        (Decq, [Reg Rdx]);
        (Jmp, [Imm (Lbl "inner")]);
      ]};

    { lbl = "inner"; global = false; asm = Text [
        (* while j >= 0 && a[j] > key *)
        (Cmpq, [Imm (Lit 0L); Reg Rdx]);
        (J Lt, [Imm (Lbl "insert_key")]);

        (* r09 = &a[j] *)
        (Movq, [Reg Rdx; Reg R09]);
        (Shlq, [Imm (Lit 3L); Reg R09]);
        (Addq, [Reg Rsp; Reg R09]);

        (* if a[j] <= key, break *)
        (Movq, [Ind2 R09; Reg Rbx]);
        (Cmpq, [Reg Rax; Reg Rbx]);
        (J Le, [Imm (Lbl "insert_key")]);

        (* a[j+1] = a[j] *)
        (Movq, [Reg Rbx; Ind3 (Lit 8L, R09)]);
        (Decq, [Reg Rdx]);
        (Jmp, [Imm (Lbl "inner")]);
      ]};

    { lbl = "insert_key"; global = false; asm = Text [
        (* a[j+1] = key *)
        (Incq, [Reg Rdx]);
        (Movq, [Reg Rdx; Reg R10]);
        (Shlq, [Imm (Lit 3L); Reg R10]);
        (Addq, [Reg Rsp; Reg R10]);
        (Movq, [Reg Rax; Ind2 R10]);

        (* i++ *)
        (Incq, [Reg Rcx]);
        (Jmp, [Imm (Lbl "main_loop")]);
      ]};

    { lbl = "verify"; global = false; asm = Text [
        (* Return 1 if sorted in nondecreasing order, else 0 *)
        (Movq, [Imm (Lit 0L); Reg Rcx]);
      ]};

    { lbl = "verify_loop"; global = false; asm = Text [
        (Cmpq, [Imm (Lit 4L); Reg Rcx]);
        (J Ge, [Imm (Lbl "sorted")]);

        (Movq, [Reg Rcx; Reg R08]);
        (Shlq, [Imm (Lit 3L); Reg R08]);
        (Addq, [Reg Rsp; Reg R08]);

        (Movq, [Ind2 R08; Reg Rax]);
        (Movq, [Ind3 (Lit 8L, R08); Reg Rbx]);
        (Cmpq, [Reg Rax; Reg Rbx]);
        (J Lt, [Imm (Lbl "not_sorted")]);

        (Incq, [Reg Rcx]);
        (Jmp, [Imm (Lbl "verify_loop")]);
      ]};

    { lbl = "sorted"; global = false; asm = Text [
        (Movq, [Imm (Lit 1L); Reg Rax]);
        (Addq, [Imm (Lit 40L); Reg Rsp]);
        (Retq, []);
      ]};

    { lbl = "not_sorted"; global = false; asm = Text [
        (Movq, [Imm (Lit 0L); Reg Rax]);
        (Addq, [Imm (Lit 40L); Reg Rsp]);
        (Retq, []);
      ]};
  ]

let insertion_sort_prog : prog = mk_insertion_sort_prog 5L 2L 4L 6L 1L
let insertion_sort_already_sorted_prog : prog = mk_insertion_sort_prog 1L 2L 3L 4L 5L
let insertion_sort_reverse_prog : prog = mk_insertion_sort_prog 9L 7L 5L 3L 1L
let insertion_sort_duplicates_prog : prog = mk_insertion_sort_prog 3L 1L 3L 2L 1L
let insertion_sort_with_negatives_prog : prog = mk_insertion_sort_prog (-2L) 4L 0L (-5L) 3L

let run_and_expect_sorted (label : string) (p : prog) : unit =
  let exec = assemble p in
  let mach = load exec in
  let r = run mach in
  if r <> 1L then
    failwith (Printf.sprintf "%s failed: got %Ld, expected 1" label r)

let provided_tests : suite = [
  Test ("Insertion sort program tests", [
    "sorts [5;2;4;6;1]", (fun () -> run_and_expect_sorted "base case" insertion_sort_prog);
    "keeps [1;2;3;4;5] sorted", (fun () -> run_and_expect_sorted "already sorted case" insertion_sort_already_sorted_prog);
    "sorts reverse [9;7;5;3;1]", (fun () -> run_and_expect_sorted "reverse sorted case" insertion_sort_reverse_prog);
    "sorts duplicates [3;1;3;2;1]", (fun () -> run_and_expect_sorted "duplicates case" insertion_sort_duplicates_prog);
    "sorts negatives [-2;4;0;-5;3]", (fun () -> run_and_expect_sorted "negatives case" insertion_sort_with_negatives_prog);
  ]);
]