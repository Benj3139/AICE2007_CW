(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86

module Platform = Util.Platform

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understanding this entire file and
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page.
*)


(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge



(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m


(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip) %rax.

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).
*)
let compile_operand (ctxt:ctxt) (dest:X86.operand) : Ll.operand -> ins list = 
    function
    (*finds stack slot of %uid using layout, loads stack value into dest*)
    | Id uid -> 
        let src = lookup ctxt.layout uid in
        begin match src, dest with
            | Reg _, Reg _ -> [(Movq, [src;dest])]
            | Reg _, Ind3 _ -> [(Movq, [src;dest])]
            | Ind3 _, Reg _ -> [(Movq, [src;dest])]
            | Ind3 _, Ind3 _ ->
                let tmp = Reg Rax in
                [(Movq, [src;tmp]); (Movq, [tmp;dest])]
        end
    (*loads address of gid (a global variable) into dest*)
    | Gid gid -> [(Leaq, [Ind3 (Lbl (Platform.mangle gid), Rip); dest])]
    (*moves integer literal into dest*)
    | Const c -> [(Movq, [Imm (Lit c); dest])]
    (*moves null pointer (0) into dest*)
    | Null -> [(Movq, [Imm (Lit 0L); dest])]



(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: Don't forget to preserve caller-save registers (only if needed). ]

   [ NOTE: Remember, call can use labels as immediates! You shouldn't need to 
     perform any RIP-relative addressing for this one. ]

   [ NOTE: It is the caller's responsibility to clean up arguments pushed onto
     the stack, so you must free the stack space after the call returns. (But 
     see below about alignment.) ]

   [ NOTE: One important detail about the ABI besides the conventions is that, 
  at the time the [callq] instruction is invoked, %rsp *must* be 16-byte aligned.  
  However, because LLVM IR provides the Alloca instruction, which can dynamically
  allocate space on the stack, it is hard to know statically whether %rsp meets
  this alignment requirement.  Moroever: since, according to the calling 
  conventions, stack arguments numbered > 6 are pushed to the stack, we must take
  that into account when enforcing the alignment property.  

  We suggest that, for a first pass, you *ignore* %rsp alignment -- only a few of 
  the test cases rely on this behavior.  Once you have everything else working,
  you can enforce proper stack alignment at the call instructions by doing 
  these steps: 
    1. *before* pushing any arguments of the call to the stack, ensure that the
    %rsp is 16-byte aligned.  You can achieve that with the x86 instruction:
    `andq $-16, %rsp`  (which zeros out the lower 4 bits of %rsp, possibly 
    "allocating" unused padding space on the stack)

    2. if there are an *odd* number of arguments that will be pushed to the stack
    (which would break the 16-byte alignment because stack slots are 8 bytes),
    allocate an extra 8 bytes of padding on the stack. 
    
    3. follow the usual calling conventions - any stack arguments will still leave
    %rsp 16-byte aligned

    4. after the call returns, in addition to freeing up the stack slots used by
    arguments, if there were an odd number of slots, also free the extra padding. 
    
  ]
*)




(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelementptr, you must generate x86 code that performs
   the appropriate arithmetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes.
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty (tdecls:(tid * ty) list) (t:Ll.ty) : int =
failwith "size_ty not implemented"




(* Generates code that computes a pointer value.

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

     - if t is a struct, the index must be a constant n and it
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the
       sizes of the types of the previous elements ]

     - if t is an array, the index can be any operand, and its
       value determines the offset within the array.

     - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)
let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
failwith "compile_gep not implemented"



(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)

let arg_loc (n : int) : operand =
    match n with
    (*first 6 arguments are passed in registers, in folowing order according to ABI*)
    | 0 -> Reg Rdi
    | 1 -> Reg Rsi
    | 2 -> Reg Rdx
    | 3 -> Reg Rcx
    | 4 -> Reg R08
    | 5 -> Reg R09
    (* arguments beyond first 6 are passed on the stack
       rbp+0 = old rbp
       rbp+8 = return address
       rbp+16 = arg6
       rbp+24 = arg7
       ... *)
    | _ -> Ind3 (Lit (Int64.of_int (16+(n-6)*8)), Rbp)


let compile_insn (ctxt:ctxt) ((uid:uid), (i:Ll.insn)) : X86.ins list =
      match i with

      | Binop (bop, _, op1, op2) ->
          (*finds stack slot for current %uid where result should be stored*)
          let dest = lookup ctxt.layout uid in

          (*register selection*)
          let r1 = Reg Rax in
          let r2 = Reg Rbx in

          (*move operands into registers*)
          let code1 = compile_operand ctxt r1 op1 in
          let code2 = compile_operand ctxt r2 op2 in

          let bop_code = match bop with
              | Add -> [Addq, [r2;r1]]
              | Sub -> [Subq, [r2;r1]]
              | Mul -> [Imulq, [r2;r1]]
              | Shl -> [Movq, [r2; Reg Rcx]] @ [(Shlq, [Reg Rcx; r1])] (*shift via rcx*)
              | Lshr -> [Movq, [r2; Reg Rcx]] @ [(Shrq, [Reg Rcx; r1])]
              | Ashr -> [Movq, [r2; Reg Rcx]] @ [(Sarq, [Reg Rcx; r1])]
              | And -> [Andq, [r2;r1]]
              | Or -> [Orq, [r2;r1]]
              | Xor -> [Xorq, [r2;r1]]
          in

          (*concatenate all instructions, store result from %rax to stack location for %uid*)
          code1 @ code2 @ bop_code @ [(Movq, [r1;dest])]

      | Icmp (cnd, _, op1, op2) ->
          let dest = lookup ctxt.layout uid in

          let r1 = Reg Rax in
          let r2 = Reg Rbx in

          let code1 = compile_operand ctxt r1 op1 in
          let code2 = compile_operand ctxt r2 op2 in

          (*perform comparison r1-r2*)
          let cmp = [(Cmpq, [r2;r1])] in

          let r08 = Reg R08 in
          let set_instr = [(Set (compile_cnd cnd), [r08])] in
          let move_to_r1 = [Movq, [r08;r1]] in

          code1 @ code2 @ cmp @ set_instr @ move_to_r1 @ [(Movq, [r1;dest])]

      | Alloca _ ->
          let dest = lookup ctxt.layout uid in
          (*[]*)
          
          
          let r = Reg Rax in

          begin match dest with
          | Ind3 (Lit offset, Rbp) -> [
                (Leaq, [Ind3 (Lit offset, Rbp); r]);
                (Movq, [r; dest])
            ]
          | _ ->
              failwith "compile_insn: Alloca unexpected operand form"
          end
          
          

      | Store (_, op_val, op_ptr) ->
          let r_val = Reg Rax in
          let r_ptr = Reg Rbx in
          
          let code_val = compile_operand ctxt r_val op_val in
          let code_ptr = compile_operand ctxt r_ptr op_ptr in
          
          (*if pointer is a memory reference, store via temporary register*)
          (*
          let store_instr = match r_ptr with
              | Reg _ -> [(Movq, [r_val; Ind2 Rbx])]
              | _ ->
                  let tmp = Reg Rcx in 
                  [(Movq, [r_val; tmp]); (Movq, [tmp; Ind2 Rbx])]
          in
          *)
          (*store_instr = [(Movq, [r_val; Ind2 r_ptr])] in*)

          code_val @ code_ptr @ [(Movq, [r_val; Ind2 Rbx])]
          (*
          let dst = match op_ptr with
              | Id uid -> lookup ctxt.layout uid
              | Gid gid -> Ind3 (Lbl (Platform.mangle gid), Rip)
              | _ -> failwith "Store: unexpected pointer"
          in

          code_val @ [(Movq, [r_val; dst])]
          *)

      | Load (_, op_ptr) ->
          let r_ptr = Reg Rax in
          let code_ptr = compile_operand ctxt r_ptr op_ptr in
          let dest = lookup ctxt.layout uid in
          (*let code_ptr = compile_operand ctxt r_tmp op_ptr in*)
          code_ptr @ [(Movq, [Ind2 Rax; dest])]
          
          (*
          let r_val = Reg Rax in
          let src = match op_ptr with
              | Id uid -> lookup ctxt.layout uid
              | Gid gid -> Ind3 (Lbl (Platform.mangle gid), Rip)
              | _ -> failwith "Load: unexpected pointer"
          in
          [(Movq, [src; r_val])]
          *)

          (*
          let dest = lookup ctxt.layout uid in
          let r_val = Reg Rax in
          let r_ptr = Reg Rbx in

          (compile_operand ctxt r_ptr op) @ 
          [(Movq, [Ind2 Rbx; r_val]); (Movq, [r_val; dest])]
          *)

      | Call (ret_ty, fn, args) ->

          let arg_moves = List.flatten(List.mapi (fun i (_,op) ->
                let dst = arg_loc i in
                compile_operand ctxt dst op
                ) args
          ) in

          let call_insn = match fn with
              | Gid gid -> [Callq, [Imm (Lbl (Platform.mangle gid))]]
              | _ -> failwith "Call: only global functions supported"
          in

          let ret_code = match ret_ty with
              | Void -> []
              | _ ->
                  let dest = lookup ctxt.layout uid in
                  [Movq, [Reg Rax; dest]]
          in

          arg_moves @ call_insn @ ret_code

      | Bitcast (_, op, _) ->
          let dest = lookup ctxt.layout uid in
          compile_operand ctxt dest op

      (*Gep*)
      

      | _ ->
          failwith "compile_insn: instruction not implemented"



(* compiling terminators  --------------------------------------------------- *)

(* prefix the function name [fn] to a label to ensure that the X86 labels are 
   globally unique . *)
let mk_lbl (fn:string) (l:string) = fn ^ "." ^ l

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional

   [fn] - the name of the function containing this terminator
*)

let compile_terminator (fn:string) (ctxt:ctxt) (t:Ll.terminator) : ins list =
    match t with

    (* return void *)
    | Ret (Void, None) ->
        [
            (*restore stack pointer: rsp<-rbp, frees local variables*)
            (Movq, [Reg Rbp;Reg Rsp]);
            (*restore caller's frame pointer*)
            (Popq, [Reg Rbp]);
            (*return to caller*)
            (Retq, [])
        ]

    (* return with value *)
    | Ret (_, Some op) ->
        (compile_operand ctxt (Reg Rax) op) @ [
            (*restore stack frame, same as return void*)
            (Movq, [Reg Rbp;Reg Rsp]);
            (Popq, [Reg Rbp]);
            (Retq, [])
        ]

    (* unconditional branch *)
    | Br lbl -> [(Jmp, [Imm (Lbl (mk_lbl fn lbl))])]

    (* conditional branch *)
    | Cbr (cond, ltrue, lfalse) ->
        (compile_operand ctxt (Reg Rax) cond) @ [
            (*compare condition with 0*)
            (Cmpq, [Imm(Lit 0L); Reg Rax]);
            (*jump if not zero (true)*)
            (J Neq, [Imm (Lbl (mk_lbl fn ltrue))]);
            (*otherwise jump to false*)
            (Jmp, [Imm (Lbl (mk_lbl fn lfalse))])
        ]

    | _ ->
        failwith "compile_terminator: unsupported terminator"
    


(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete. 
   [fn] - the name of the function containing this block
   [ctxt] - the current context
   [blk]  - LLVM IR code for the block
*)
let compile_block (fn:string) (ctxt:ctxt) (blk:Ll.block) : ins list =
    (*compile all instructions in the block*)
    (*flatten since each LLVM instruction -> list of X86 instructions*)
    let insn_code = List.flatten (
        List.map (fun insn ->
            compile_insn ctxt insn    
        ) blk.insns
    ) in

    let (_,term) = blk.term in

    (*combine with compileed terminator*)
    insn_code @ compile_terminator fn ctxt term


let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)


(* Complete this helper function, which computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions. We will test this function as part of
   the hidden test cases.

   You might find it useful for compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)


(* We suggest that you create a helper function that computes the
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id
     is also stored as a stack slot.
   - see the discussion about locals

*)
let stack_layout (args : uid list) ((block, lbled_blocks):cfg) : layout =
    (*
        - Computes stack layout for a given function declaration
        - The layout maps each LLVM local identifier (uid) to a stack slot
          located at some offset from rbp
    *)
    (*helper that extracts instruction uids from a block, ignore terminator*)
    let uids_from_block blk = List.map fst blk.insns in

    let entry_uids = uids_from_block block in

    let labeled_uids = List.flatten (
        List.map (
            (*ignore label, extract uids from each label block*)
            fun (_,blk) -> uids_from_block blk) lbled_blocks
    ) in

    (*combine all variables for stack allocation*)
    let all_uids = args @ entry_uids @ labeled_uids in
    List.mapi (fun i uid ->
        (*each slot is 8 bytes, compute -ve offset for uid stack slot*)
        let offset = -8*(i+1) in
        (*map uid to memory location [rbp+offset]*)
        (uid, Ind3 (Lit (Int64.of_int offset), Rbp))
    ) all_uids
    

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl (tdecls:(tid * ty) list) (name:string) ({ f_param; f_cfg; _ }:fdecl) : prog =
    (*compute layout using helper*)
    let layout = stack_layout f_param f_cfg in
    (*build context*)
    let ctxt = { tdecls; layout } in

    (*compute stack space needed based on number of entries in layout*)
    let stack_size = 8*(List.length layout) in

    (*function prologue*)
    let prologue = [
        (*save old frame*)
        (Pushq, [Reg Rbp]);
        (*start a new frame*)
        (Movq, [Reg Rsp; Reg Rbp]);
        (*allocate space for variables*)
        (Subq, [Imm (Lit (Int64.of_int stack_size)); Reg Rsp])
    ] in

    (*move incoming arguments from registers to stack slots*)
    let move_args = List.flatten (
        List.mapi (fun i uid ->
            let src = arg_loc i in
            let dst = List.assoc uid layout in
            match src, dst with
            | (Ind3 _ | Ind2 _), (Ind3 _ | Ind2 _) ->
                let r = Reg Rax in
                [(Movq, [src; r]); (Movq, [r; dst])]
            | _ ->
                [(Movq, [src;dst])]
        ) f_param
    ) in

    let (entry_block, labeled_blocks) = f_cfg in

    (* compile entry block with unique "entry" label *)
    let entry_elem = 
      let entry_code = compile_block name ctxt entry_block in
      Asm.gtext name (*(mk_lbl name "entry")*) (prologue @ move_args @ entry_code)
    in

    (* compile the rest of the labeled blocks *)
    let labeled_elems = List.map (fun (lbl, blk) ->
      compile_lbl_block name lbl ctxt blk
    ) labeled_blocks in

    entry_elem :: labeled_elems


(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (_t1,g,_t2) -> compile_ginit g

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls; _} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)