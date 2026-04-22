open Ll
open Llutil
open Ast

(* This file is where much of the work of the project will be carried out.
   Follow the instructions on the project web site, but first skim through
   this file to see what it contains.  
*)


(* instruction streams ------------------------------------------------------ *)

(* As in the Simple compiler demonstrated in lecture, we'll be working with a
   flattened representation of LLVMlite programs to make emitting code
   easier. This version additionally makes it possible to emit elements that
   will be gathered up and "hoisted" to specific parts of the constructed CFG

   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals

   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for
     compiling local variable declarations

   You may want to refresh your memory about the ir3.ml file from lec06 where
   this CFG construction algorithm was described.
*)

type elt =
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

(* The type of streams of LLVMLite instructions. Note that to improve performance,
 * we will emit the instructions in reverse order. That is, the LLVMLite code:
 *     %1 = mul i64 2, 2
 *     %2 = add i64 1, %1
 *     br label %l1
 * would be constructed as a stream as follows:
 *         I ("1", Binop (Mul, I64, Const 2L, Const 2L))
 *     >:: I ("2", Binop (Add, I64, Const 1L, Id "1"))
 *     >:: T (Br "l1")
 *)
type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None ->
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some term ->
              (gs, einsns, [], None, (l, {insns; term})::blks)
           end
        | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator"
    | Some term ->
       let insns = einsns @ insns in
       ({insns; term}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    try
      List.assoc id c
    with
    | Not_found -> failwith @@ Printf.sprintf "ERROR: identifier %s not found in context." id

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None
 
end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) ->
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* Compiler Invariants

   The LLVM IR type of a variable (whether global or local) that stores an Oat
   array value (or any other reference type, like "string") will always be a
   double pointer.  In general, any Oat variable of Oat-type t will be
   represented by an LLVM IR value of type Ptr (cmp_ty t).  So the Oat variable
   x : int will be represented by an LLVM IR value of type i64*, y : string will
   be represented by a value of type i8**, and arr : int[] will be represented
   by a value of type {i64, [0 x i64]}**.  Whether the LLVM IR type is a
   "single" or "double" pointer depends on whether t is a reference type.

   We can think of the compiler as paying careful attention to whether a piece
   of Oat syntax denotes the "value" of an expression or a pointer to the
   "storage space associated with it".  This is the distinction between an
   "expression" and the "left-hand-side" of an assignment statement.  Compiling
   an Oat variable identifier as an expression ("value") does the load, so
   cmp_exp called on an Oat variable of type t returns (code that) generates a
   LLVM IR value of type cmp_ty t.  Compiling an identifier as a left-hand-side
   does not do the load, so cmp_lhs called on an Oat variable of type t returns
   and operand of type (cmp_ty t)*.  Extending these invariants to account for
   array accesses: the assignment e1[e2] = e3; treats e1[e2] as a
   left-hand-side, so we compile it as follows: compile e1 as an expression to
   obtain an array value (which is of pointer of type {i64, [0 x s]}* ).
   compile e2 as an expression to obtain an operand of type i64, generate code
   that uses getelementptr to compute the offset from the array value, which is
   a pointer to the "storage space associated with e1[e2]".

   On the other hand, compiling e1[e2] as an expression (to obtain the value of
   the array), we can simply compile e1[e2] as a left-hand-side and then do the
   load.  So cmp_exp and cmp_lhs are mutually recursive.  Compiling the
   "inclusion" of an lhs as an expression (the Lhs constructor) generates the
   load.

 
   Consider globals7.oat (in hw4programs)

   /--------------- globals7.oat ------------------
   global arr = int[] null;

   int foo() {
     var x = new int[3];
     arr = x;
     x[2] = 3;
     return arr[2];
   }
   /------------------------------------------------

   The translation (given by cmp_ty) of the type int[] is {i64, [0 x i64]}* so
   the corresponding LLVM IR declaration will look like:

   @arr = global { i64, [0 x i64] }* null

   This means that the type of the LLVM IR identifier @arr is {i64, [0 x i64]}**
   which is consistent with the type of a locally-declared array variable.

   The local variable x would be allocated and initialized by (something like)
   the following code snippet.  Here %_x7 is the LLVM IR uid containing the
   pointer to the "storage space" for the Oat variable x.

   %_x7 = alloca { i64, [0 x i64] }*                              ;; (1)
   %_raw_array5 = call i64*  @oat_alloc_array(i64 3)              ;; (2)
   %_array6 = bitcast i64* %_raw_array5 to { i64, [0 x i64] }*    ;; (3)
   store { i64, [0 x i64]}* %_array6, { i64, [0 x i64] }** %_x7   ;; (4)

   (1) note that alloca uses cmp_ty (int[]) to find the type, so %_x7 has
       the same type as @arr

   (2) @oat_alloc_array allocates len+1 i64's

   (3) we have to bitcast the result of @oat_alloc_array so we can store it
        in %_x7

   (4) stores the resulting array value (itself a pointer) into %_x7

  The assignment arr = x; gets compiled to (something like):

  %_x8 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7     ;; (5)
  store {i64, [0 x i64] }* %_x8, { i64, [0 x i64] }** @arr       ;; (6)

  (5) load the array value (a pointer) that is stored in the address pointed
      to by %_x7

  (6) store the array value (a pointer) into @arr

  The assignment x[2] = 3; gets compiled to (something like):

  %_x9 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7      ;; (7)
  %_index_ptr11 = getelementptr { i64, [0 x  i64] },
                  { i64, [0 x i64] }* %_x9, i32 0, i32 1, i32 2   ;; (8)
  store i64 3, i64* %_index_ptr11                                 ;; (9)

  (7) as above, load the array value that is stored %_x7

  (8) calculate the offset from the array using GEP

  (9) store 3 into the array

  Finally, return arr[2]; gets compiled to (something like) the following.
  Note that the way arr is treated is identical to x.  (Once we set up the
  translation, there is no difference between Oat globals and locals, except
  how their storage space is initially allocated.)

  %_arr12 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** @arr    ;; (10)
  %_index_ptr14 = getelementptr { i64, [0 x i64] },                
                 { i64, [0 x i64] }* %_arr12, i32 0, i32 1, i32 2  ;; (11)
  %_index15 = load i64, i64* %_index_ptr14                         ;; (12)
  ret i64 %_index15

  (10) just like for %_x9, load the array value that is stored in @arr

  (11)  calculate the array index offset

  (12) load the array value at the index

*)

(* Global initialized arrays:

  There is another wrinkle: to compile global initialized arrays like in the
  globals4.oat, it is helpful to do a bitcast once at the global scope to
  convert the "precise type" required by the LLVM initializer to the actual
  translation type (which sets the array length to 0).  So for globals4.oat,
  the arr global would compile to (something like):

  @arr = global { i64, [0 x i64] }* bitcast
           ({ i64, [4 x i64] }* @_global_arr5 to { i64, [0 x i64] }* )
  @_global_arr5 = global { i64, [4 x i64] }
                  { i64 4, [4 x i64] [ i64 1, i64 2, i64 3, i64 4 ] }

*)



(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.  
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate a zero-initialized array of source type TRef
   (RArray t) of the given size. Note "size" is an operand whose value can be
   computed at runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = Ptr I64 in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]




(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression.

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to make sure
     either that the resulting gid has type (Ptr I8), or, if the gid has type
     [n x i8] (where n is the length of the string), convert the gid to a
     (Ptr I8), e.g., by using getelementptr.

   - use the provided "oat_alloc_array" function to implement literal arrays
   (CArr) and the (NewArr) expressions

   - we have provided the signature for the (mutually recursive) helper
     function cmp_lhs, which should make the Lhs case easy

   - you are encouraged to add additional helper functions, e.g, to for
     compiling Call  
*)
let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  match exp.elt with
  | CNull r -> cmp_ty (TRef r), Null, []
  | CBool b -> I1, Const (if b then 1L else 0L), []
  | CInt i -> I64, Const i, []
  | CStr s ->
      let gid = gensym "str" in
      let n = String.length s + 1 in
      let arr_ty = Array (n, I8) in
      let ptr = gensym "strptr" in
      Ptr I8, Id ptr,
      [ G (gid, (arr_ty, GString s))
      ; I (ptr, Gep (arr_ty, Gid gid, [Const 0L; Const 0L])) ]
  | Lhs l ->
      let ty, lhs_op, lhs_code = cmp_lhs c l in
      let id = gensym "load" in
      ty, Id id, lhs_code >@ [I (id, Load (ty, lhs_op))]
  | CArr (t, es) ->
      let n = List.length es in
      let arr_ty, arr_op, alloc_code = oat_alloc_array t (Const (Int64.of_int n)) in
      let ety = cmp_ty t in
      let stores =
        List.mapi (fun i e ->
            let _, eop, ecode = cmp_exp c e in
            let gep_id = gensym "idx" in
            let gep_code =
              [ I (gep_id, Gep (cmp_rty (RArray t), arr_op, [Const 0L; Const 1L; Const (Int64.of_int i)]))
              ; I (gensym "store", Store (ety, eop, Id gep_id)) ]
            in
            ecode >@ gep_code
          ) es
      in
      arr_ty, arr_op, alloc_code >@ List.flatten stores
  | NewArr (t, e) ->
      let _, sz, csz = cmp_exp c e in
      oat_alloc_array t sz |> fun (ty, op, ca) -> ty, op, csz >@ ca
  | Call (fn, args) ->
      let fty, fop = Ctxt.lookup_function fn c in
      let arg_codes, ll_args =
        List.split (List.map (fun a ->
            let ty, op, code = cmp_exp c a in
            code, (ty, op)
          ) args)
      in
      begin match fty with
      | Ptr (Fun (_, rty)) ->
          let code = List.flatten arg_codes in
          if rty = Void then
            rty, Null, code >@ [I (gensym "callv", Call (rty, fop, ll_args))]
          else
            let id = gensym "call" in
            rty, Id id, code >@ [I (id, Call (rty, fop, ll_args))]
      | _ -> failwith "cmp_exp: call target is not a function"
      end
  | Bop (And, e1, e2) ->
      let _, o1, c1 = cmp_exp c e1 in
      let _, o2, c2 = cmp_exp c e2 in
      let id = gensym "and" in
      I1, Id id, c1 >@ c2 >@ [I (id, Binop (And, I1, o1, o2))]
  | Bop (Or, e1, e2) ->
      let _, o1, c1 = cmp_exp c e1 in
      let _, o2, c2 = cmp_exp c e2 in
      let id = gensym "or" in
      I1, Id id, c1 >@ c2 >@ [I (id, Binop (Or, I1, o1, o2))]
  | Bop (b, e1, e2) ->
      let _, o1, c1 = cmp_exp c e1 in
      let _, o2, c2 = cmp_exp c e2 in
      begin match b with
      | Add | Sub | Mul | Shl | Shr | Sar | IAnd | IOr ->
          let llb = match b with
            | Add -> Ll.Add | Sub -> Ll.Sub | Mul -> Ll.Mul
            | Shl -> Ll.Shl | Shr -> Ll.Lshr | Sar -> Ll.Ashr
            | IAnd -> Ll.And | IOr -> Ll.Or
            | _ -> failwith "impossible"
          in
          let id = gensym "bop" in
          I64, Id id, c1 >@ c2 >@ [I (id, Binop (llb, I64, o1, o2))]
      | Eq | Neq | Lt | Lte | Gt | Gte ->
          let cnd = match b with
            | Eq -> Ll.Eq | Neq -> Ll.Ne | Lt -> Ll.Slt
            | Lte -> Ll.Sle | Gt -> Ll.Sgt | Gte -> Ll.Sge
            | _ -> failwith "impossible"
          in
          let id = gensym "cmp" in
          I1, Id id, c1 >@ c2 >@ [I (id, Icmp (cnd, I64, o1, o2))]
      | _ -> failwith "cmp_exp: unsupported bop"
      end
  | Uop (Neg, e) ->
      let _, o, ce = cmp_exp c e in
      let id = gensym "neg" in
      I64, Id id, ce >@ [I (id, Binop (Sub, I64, Const 0L, o))]
  | Uop (Bitnot, e) ->
      let _, o, ce = cmp_exp c e in
      let id = gensym "not" in
      I64, Id id, ce >@ [I (id, Binop (Xor, I64, o, Const (-1L)))]
  | Uop (Lognot, e) ->
      let _, o, ce = cmp_exp c e in
      let id = gensym "lnot" in
      I1, Id id, ce >@ [I (id, Icmp (Eq, I1, o, Const 0L))]  

(* compiles a left-hand side: produces and intruction stream that
   yields an operand to which a value can be stored (by Assn) or
   from which a value can be loaded (by Lhs).

   Note that function identifiers *do not* correspond to valid left-hand-
   sides.
 *)
and cmp_lhs (c:Ctxt.t) (l:lhs node) : Ll.ty * Ll.operand * stream =
  match l.elt with
  | Id x ->
      let ty, op = Ctxt.lookup x c in
      begin match ty with
      | Ptr (Fun _) -> failwith "cmp_lhs: function is not assignable"
      | Ptr t ->
          (* Globals are addressable storage already. In buggy/mixed contexts
             string globals may appear as i8* instead of i8**; repair that
             here so `Lhs(Id x)` loads the string pointer, not a byte. *)
          let t' =
            match op, t with
            | Gid _, I8 -> Ptr I8
            | _ -> t
          in
          t', op, []
      | _ -> failwith "cmp_lhs: malformed context binding"
      end
  | Index (e1, e2) ->
      let arr_ty, arr_op, c1 = cmp_exp c e1 in
      let _, idx_op, c2 = cmp_exp c e2 in
      begin match arr_ty with
      | Ptr (Struct [_; Array (_, ety)]) ->
          let gid = gensym "elem" in
          ety, Id gid, c1 >@ c2 >@ [I (gid, Gep (Struct [I64; Array (0, ety)], arr_op, [Const 0L; Const 1L; idx_op]))]
      | _ -> failwith "cmp_lhs: indexing non-array"
      end


   

(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add bindings to the context for local variable
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST, and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement
 *)
let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  match stmt.elt with
  | Ret None -> c, [T (Ret (rt,None))]
  | Ret (Some e) ->
      (*
      return a pair of (
          current context unchanged,
          code to compute expression and return result
      )
      *)
      let (_, op, code) = cmp_exp c e in
      c, code >@ [T (Ret (rt,Some op))]
  | Decl (id, e) ->
      let t, op, code = cmp_exp c e in
      let slot = gensym id in
      let c' = Ctxt.add c id (Ptr t, Id slot) in
      c', code >@ [E (slot, Alloca t); I (gensym "store", Store (t, op, Id slot))]
  | Assn (l, e) ->
      let t, ptr, cl = cmp_lhs c l in
      let _, op, ce = cmp_exp c e in
      c, cl >@ ce >@ [I (gensym "store", Store (t, op, ptr))]
  | SCall (fn, args) ->
      let _, _, code = cmp_exp c (Ast.no_loc (Call (fn, args))) in
      c, code
  | If (cond, thn, els) ->
      let _, cop, cc = cmp_exp c cond in
      let l_then = gensym "then" in
      let l_else = gensym "else" in
      let l_end = gensym "ifend" in
      let _, cthen = cmp_block c rt thn in
      let _, celse = cmp_block c rt els in
      c,
      cc >@
      [T (Cbr (cop, l_then, l_else)); L l_then] >@
      cthen >@ [T (Br l_end); L l_else] >@
      celse >@ [T (Br l_end); L l_end]
  | While (cond, body) ->
      let l_cond = gensym "while_cond" in
      let l_body = gensym "while_body" in
      let l_end = gensym "while_end" in
      let _, cop, cc = cmp_exp c cond in
      let _, cbody = cmp_block c rt body in
      c, [T (Br l_cond); L l_cond] >@ cc >@
        [T (Cbr (cop, l_body, l_end)); L l_body] >@
        cbody >@
        [T (Br l_cond); L l_end]
  | For (vdecls, cond_opt, post_opt, body) ->
      let mk_decl (id, e) = Ast.no_loc (Decl (id, e)) in
      let cond = match cond_opt with Some e -> e | None -> Ast.no_loc (CBool true) in
      let body' = match post_opt with Some s -> body @ [s] | None -> body in
      let desugared =
        (List.map mk_decl vdecls) @ [Ast.no_loc (While (cond, body'))]
      in
      cmp_block c rt desugared

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : Ctxt.t * stream =
  List.fold_left (fun (c, code) s ->
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts



(* Adds each function identifer to the context at an appropriately translated
   type.

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
         let ft = TRef (RFun (List.map fst args, frtyp)) in
         Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p

(* Populate a context with bindings for global variables mapping OAT identifiers
   to LLVMlite gids and their types. *)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left (fun c -> function
    | Ast.Gvdecl { elt = { name; init } } ->
      let t =
        (*
          Only a small subset of OAT expressions can be used as global initializers
          in well-formed programs (The constructors starting with C). Fail rest
        *)
        match init.elt with
        | CNull r -> TRef r
        | CBool _ -> TBool
        | CInt _  -> TInt
        | CStr _  -> TRef RString
        | CArr (t, _) -> TRef (RArray t)
        | _ -> failwith "cmp_global_ctxt: invalid global initialiser"
      in
      Ctxt.add c name (Ptr (cmp_ty t), Gid name)
    | _ -> c
  ) c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for the function arguments
   4. Compile the body of the function using cmp_block
   5. Use cfg_of_stream to produce a LLVMlite cfg from the body
 *)
let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
    let {frtyp; fname; args; body} = f.elt in

    (*function type*)
    let arg_tys = List.map (fun (t,_) -> cmp_ty t) args in
    let ret_ty = cmp_ret_ty frtyp in
    let fty = (arg_tys,ret_ty) in

    (*parameter names*)
    let params = List.map snd args in

    (*body returning true value*)
    let c_with_args, arg_setup =
      List.fold_left2 (fun (ctx, code) aty param ->
          let slot = gensym param in
          let ctx' = Ctxt.add ctx param (Ptr aty, Id slot) in
          let code' = code >@ [E (slot, Alloca aty); I (gensym "argstore", Store (aty, Id param, Id slot))] in
          (ctx', code')
        ) (c, []) arg_tys params
    in
    let _, body_code = cmp_block c_with_args ret_ty body in
    let code = arg_setup >@ body_code in

    let cfg, gdecls = cfg_of_stream code in

    ({f_ty=fty; f_param=params; f_cfg=cfg}, gdecls)


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations.
*)
let rec cmp_gexp c (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  match e.elt with
  | CNull r -> (cmp_ty (TRef r), GNull), []
  | CBool b -> (I1, GInt (if b then 1L else 0L)), []
  | CInt i -> (I64, GInt i), []
  | CStr s ->
      let n = String.length s + 1 in
      let str_gid = gensym "str" in
      let str_ty = Array (n, I8) in
      let str_decl = (str_ty, GString s) in
      (Ptr I8, GBitcast (Ptr str_ty, GGid str_gid, Ptr I8)), [str_gid, str_decl]
  | CArr (t, es) ->
      let ety = cmp_ty t in
      let compiled = List.map (cmp_gexp c) es in
      let elem_inits =
          List.map (fun ((_, ginit), _) -> (ety, ginit)) compiled
      in
      let nested_gdecls = List.concat (List.map snd compiled) in
      let n = List.length es in
      let arr_storage_ty = Struct [I64; Array (n, ety)] in
      let arr_storage_init =
        GStruct [
          I64, GInt (Int64.of_int n);
          Array (n, ety), GArray elem_inits
        ]
      in
      let arr_gid = gensym "garr" in
      let arr_ty = cmp_ty (TRef (RArray t)) in
      (arr_ty, GBitcast (Ptr arr_storage_ty, GGid arr_gid, arr_ty)),
      (arr_gid, (arr_storage_ty, arr_storage_init)) :: nested_gdecls
  | _ ->
      failwith "cmp_gexp: invalid global initializer"


(* Oat internals function context ------------------------------------------- *)
let internals = [
    "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt =
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls =
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } ->
           let ll_gd, gs' = cmp_gexp c gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }