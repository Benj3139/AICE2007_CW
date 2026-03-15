(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | _ -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* PART I TASK 1 *)
(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool =
    function
    | Eq -> fz
    | Neq -> not fz
    | Lt -> fs<>fo
    | Le -> fz || (fs<>fo)
    | Gt -> (not fz) && (fs=fo)
    | Ge -> fs=fo

(* PART I TASK 2 *)
(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
    if Int64.compare addr mem_bot < 0 || Int64.compare addr mem_top >= 0 then
        None
    else
        (* Memory array starts at idx 0, but lowest x86 address = mem_bot
           quad is int64 so must do Int64.sub, then convert to int*)
        Some(Int64.to_int(Int64.sub addr mem_bot))

(* PART I TASK 3 *)
(* Third, implement the interpretation of operands (including indirect
   addresses), since this functionality will be needed for simulating 
   instructions. *)

(* Effective address *)
let eval_addr (m:mach) (op:operand) : quad =
    match op with
    | Ind1 (Lit i) -> i
    | Ind2 r -> m.regs.(rind r)
    | Ind3 (Lit i, r) -> Int64.add i m.regs.(rind r)
    | _ -> failwith "invalid address operand"

(* Read operand value *)
let read_operand (m:mach) (op:operand) : quad =
    match op with
    | Imm (Lit i) -> i
    | Imm (Lbl l) -> failwith ("read_operand: unexpected label")
    | Reg r -> m.regs.(rind r)
    | Ind1 _ | Ind2 _ | Ind3 _ ->
        let addr = eval_addr m op in
        match map_addr addr with
        | Some idx -> int64_of_sbytes(Array.to_list(Array.sub m.mem idx 8))
        | None -> raise X86lite_segfault

(* Write operand *)
let write_operand (m:mach) (op:operand) (v:quad) : unit =
    match op with
    | Reg r -> m.regs.(rind r) <- v
    | Ind1 _ | Ind2 _ | Ind3 _ ->
        let addr = eval_addr m op in
        begin match map_addr addr with
        | Some idx -> 
            let bs = sbytes_of_int64 v in Array.blit(Array.of_list bs) 0 m.mem idx 8
        | None -> raise X86lite_segfault
        end
    | Imm _ -> failwith "cannot write to immediate"

(* PART I TASK 4 *)
(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)

(* helper: fetch instruction *)
let fetch (m:mach) : ins =
    let rip = m.regs.(rind Rip) in
    match map_addr rip with
    | Some idx ->
        begin match m.mem.(idx) with
        | InsB0 ins -> ins
        | _ -> failwith "not an instruction"
        end
    | None -> raise X86lite_segfault

(* helper: next instruction address *)
let next_rip (m:mach) : quad =
    Int64.add m.regs.(rind Rip) ins_size

(* bit manipulation helper - checking AMT value*)
let get_shift_amount m = function
    | Imm (Lit i) -> Int64.to_int i
    | Reg Rcx -> Int64.to_int m.regs.(rind Rcx)
    | _ -> failwith "invalid shift amount"

let step (m:mach) : unit =
    (*FETCH INSTRUCTION*)
    let (op, args) = fetch m in
    let rip_next = next_rip m in

    (*DECODE & EXECUTE INSTRUCTION*)
    match op, args with
    (* ===== arithmetic instructions ===== *)
    (*dest = -dest*)
    | Negq, [dest] ->
        let v = read_operand m dest in
        let r = Int64_overflow.neg v in
        write_operand m dest r.value;
        m.flags.fo <- r.overflow; (*overflow if dest = -2^63, can't represent 2^63*)
        m.flags.fz <- (r.value=0L);
        m.flags.fs <- (r.value<0L);
        m.regs.(rind Rip) <- rip_next
    | Addq, [src; dest] ->
        let a = read_operand m dest in
        let b = read_operand m src in
        let r = Int64_overflow.add a b in
        write_operand m dest r.value;
        m.flags.fo <- r.overflow;
        m.flags.fz <- (r.value=0L);
        m.flags.fs <- (r.value<0L);
        m.regs.(rind Rip) <- rip_next
    | Subq, [src; dest] ->
        let a = read_operand m dest in
        let b = read_operand m src in
        let r = Int64_overflow.sub a b in
        write_operand m dest r.value;
        m.flags.fo <- r.overflow;
        m.flags.fz <- (r.value=0L);
        m.flags.fs <- (r.value<0L);
        m.regs.(rind Rip) <- rip_next
    | Imulq, [src; Reg r] ->
        let a = m.regs.(rind r) in
        let b = read_operand m src in
        let result = Int64_overflow.mul a b in
        m.regs.(rind r) <- result.value;
        m.flags.fo <- result.overflow;
        m.regs.(rind Rip) <- rip_next
    | Incq, [dest] ->
        let v = read_operand m dest in
        let r = Int64_overflow.add v 1L in
        write_operand m dest r.value;
        m.flags.fo <- r.overflow;
        m.flags.fz <- (r.value=0L);
        m.flags.fs <- (r.value<0L);
        m.regs.(rind Rip) <- rip_next
    | Decq, [dest] ->
        let v = read_operand m dest in
        let r = Int64_overflow.sub v 1L in
        write_operand m dest r.value;
        m.flags.fo <- r.overflow;
        m.flags.fz <- (r.value=0L);
        m.flags.fs <- (r.value<0L);
        m.regs.(rind Rip) <- rip_next

    (* ===== logic instructions ===== *)
    | Notq, [dest] ->
        let v = read_operand m dest in
        let res = Int64.lognot v in
        write_operand m dest res;
        m.regs.(rind Rip) <- rip_next
    | Andq, [src; dest] ->
        let a = read_operand m dest in
        let b = read_operand m src in
        let res = Int64.logand a b in
        write_operand m dest res;
        m.flags.fo <- false;
        m.flags.fz <- (res=0L);
        m.flags.fs <- (res<0L);
        m.regs.(rind Rip) <- rip_next
    | Orq, [src; dest] ->
        let a = read_operand m dest in
        let b = read_operand m src in
        let res = Int64.logor a b in
        write_operand m dest res;
        m.flags.fo <- false;
        m.flags.fz <- (res=0L);
        m.flags.fs <- (res<0L);
        m.regs.(rind Rip) <- rip_next
    | Xorq, [src; dest] ->
        let a = read_operand m dest in
        let b = read_operand m src in
        let res = Int64.logxor a b in
        write_operand m dest res;
        m.flags.fo <- false;
        m.flags.fz <- (res=0L);
        m.flags.fs <- (res<0L);
        m.regs.(rind Rip) <- rip_next
        
    (* ===== bit-manipulation instructions ===== *)
    (* arithmetic right shift *)
    | Sarq, [amt; dest] ->
        (*AMT must be a Imm or rcx operand*)
        let shift = get_shift_amount m amt in
        (*if AMT=0, flags unchanged*)
        if shift<>0 then (
            let v = read_operand m dest in
            let res = Int64.shift_right v shift in
            write_operand m dest res;
            m.flags.fs <- (res<0L);
            m.flags.fz <- (res=0L);
            (*If AMT=1 set OF flag to 0, else unchanged*)
            if shift=1 then m.flags.fo <- false
        );
        m.regs.(rind Rip) <- rip_next
    (* bitwise left shift *)
    | Shlq, [amt; dest] ->
        let shift = get_shift_amount m amt in
        (*If AMT=0, unchanged*)
        if shift<>0 then (
            let v = read_operand m dest in
            let res = Int64.shift_left v shift in
            write_operand m dest res;
            m.flags.fs <- (res<0L);
            m.flags.fz <- (res=0L);

            (*compare two MSBs, only change OF appropriately if shift=1*)
            if shift=1 then (
                (*shifts MSB into first bit*)
                let msb = Int64.shift_right_logical v 63 in
                (*shifts 2nd MSB into first bit, second bit contains MSB still*)
                let msb_pair = Int64.shift_right_logical v 62 in
                (*mask required to extract only 2nd MSB*)
                let second_msb = Int64.logand msb_pair 1L in
                (*OF flag -> 0 if msb=second_msb*)
                m.flags.fo <- (msb<>second_msb)
            )
        );
        m.regs.(rind Rip) <- rip_next
    (* Bitwise right shift *)
    | Shrq, [amt; dest] ->
        let shift = get_shift_amount m amt in
        (*If AMT=0, unchanged*)
        if shift<>0 then (
            let v = read_operand m dest in
            let res = Int64.shift_right_logical v shift in
            write_operand m dest res;
            (*FS flag -> result MSB*)
            m.flags.fs <- (Int64.shift_right_logical res 63 = 1L);
            m.flags.fz <- (res=0L);

            if shift=1 then (
                (*OF flag -> initial MSB*)
                m.flags.fo <- (Int64.shift_right_logical v 63 = 1L)
            )
        );
        m.regs.(rind Rip) <- rip_next
    (*Set byte*)
    | Set c, [dest] ->
        let cond = interp_cnd m.flags c in
        let v = read_operand m dest in
        let byte = if cond then 1L else 0L in
        let res = Int64.logor (Int64.logand v 0xffffffffffffff00L) byte in
        write_operand m dest res;
        m.regs.(rind Rip) <- rip_next

    (* Data movement instructions *)
    | Leaq, [ind; dest] ->
        let addr = read_operand m ind in
        write_operand m dest addr;
        m.regs.(rind Rip) <- rip_next
    | Movq, [src; dest] ->
        let v = read_operand m src in
        write_operand m dest v;
        m.regs.(rind Rip) <- rip_next
    | Pushq, [src] ->
        (*decrement stack pointer*)
        m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
        let addr = m.regs.(rind Rsp) in
        write_operand m (Ind1 (Lit addr)) (read_operand m src);
        m.regs.(rind Rip) <- rip_next
    | Popq, [dest] ->
        let addr = m.regs.(rind Rsp) in
        let v = read_operand m (Ind1 (Lit addr)) in
        write_operand m dest v;
        m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L;
        m.regs.(rind Rip) <- rip_next

    (* Control flow and condition instructions *)
    | Cmpq, [src1; src2] ->
        (*check that src2 is not an immediate*)
        begin match src2 with
        | Imm _ -> invalid_arg "cmpq: SRC2 cannot be an immediate"
        | _ ->
            let v1 = read_operand m src1 in
            let v2 = read_operand m src2 in
            let r = Int64_overflow.sub v2 v1 in
            m.flags.fo <- r.overflow;
            m.flags.fz <- (r.value=0L);
            m.flags.fs <- (r.value<0L);
            m.regs.(rind Rip) <- rip_next
        end
    | Jmp, [src] ->
        m.regs.(rind Rip) <- read_operand m src
    | Callq, [src] ->
        (*Calls a procedure*)
        (*push address of next instruction onto stack*)
        m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
        let return_addr = rip_next in
        write_operand m (Ind1 (Lit m.regs.(rind Rsp))) return_addr;
        (*jump*)
        m.regs.(rind Rip) <- read_operand m src
    | Retq, [] ->
        (*pop address from top of stack into rip*)
        let addr = read_operand m (Ind1 (Lit m.regs.(rind Rsp))) in
        (*increment rsp by 8*)
        m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L;
        m.regs.(rind Rip) <- addr
    | J c, [src] ->
        if interp_cnd m.flags c then
            m.regs.(rind Rip) <- read_operand m src
        else
            m.regs.(rind Rip) <- rip_next
    | _ -> failwith "Unsupported instruction"


(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
