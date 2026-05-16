open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) (err : string) = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with
  | TBool, TBool
  | TInt, TInt -> true
  | TRef r1, TRef r2 -> subtype_ref c r1 r2
  | TRef r1, TNullRef r2 -> subtype_ref c r1 r2
  | TNullRef r1, TNullRef r2 -> subtype_ref c r1 r2
  | _, _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match t1, t2 with
  | RString, RString -> true
  | RArray t1, RArray t2 -> subtype c t1 t2 && subtype c t2 t1
  | RStruct id1, RStruct id2 ->
      if id1 = id2 then true
      else begin
        match lookup_struct_option id1 c, lookup_struct_option id2 c with
        | Some fs1, Some fs2 ->
            List.for_all (fun f2 ->
              match List.find_opt (fun f1 -> f1.fieldName = f2.fieldName) fs1 with
              | None -> false
              | Some f1 -> subtype c f1.ftyp f2.ftyp
            ) fs2
        | _ -> false
      end
  | RFun (args1, ret1), RFun (args2, ret2) ->
      List.length args1 = List.length args2
      && List.for_all2 (fun a2 a1 -> subtype c a2 a1) args2 args1
      && subtype_ret c ret1 ret2
  | _, _ -> false

and subtype_ret (c : Tctxt.t) (r1 : Ast.ret_ty) (r2 : Ast.ret_ty) : bool =
  match r1, r2 with
  | RetVoid, RetVoid -> true
  | RetVal t1, RetVal t2 -> subtype c t1 t2
  | _, _ -> false


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TBool | TInt -> ()
  | TRef rt | TNullRef rt -> typecheck_rty l tc rt

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.rty) : unit =
  match rt with
  | RString -> ()
  | RStruct id ->
      begin match lookup_struct_option id tc with
      | Some _ -> ()
      | None -> type_error l ("Unknown struct type: " ^ id)
      end
  | RArray t -> typecheck_ty l tc t
  | RFun (args, r) ->
      List.iter (typecheck_ty l tc) args;
      typecheck_ret_ty l tc r

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.ret_ty) : unit =
  match rt with
  | RetVoid -> ()
  | RetVal t -> typecheck_ty l tc t


(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t : Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _ -> false

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  failwith "todo: implement typecheck_exp"

(* Typechecks a lhs expression in the typing context c.  Returns the
   type of result, along with a boolean flag indicating whether
   the lhs is "assignable".
   INVARIANT:
     If the flag is true, we can think of lhs as denoting a reference
     to a value of the returned type.

     If the flag is false, the lhs is a (globally defined) function
     pointer (which cannot be written to).
 *)
and typecheck_lhs (c : Tctxt.t) (l : Ast.lhs node) : Ast.ty * bool =
  failwith "todo: implement typecheck_lhs"

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)

     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, the return behavior of the branching 
        statement is the conjunction of the return behavior of the two 
        branches: both both branches must definitely return in order for 
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entire conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  failwith "todo: implement typecheck_stmt"


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups (fs : field list) =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) (id : id) (fs : field list)  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - ensures formal parameters are distinct
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  let seen = Hashtbl.create 16 in
  List.iter (fun (t, id) ->
    if Hashtbl.mem seen id then type_error l ("Duplicate argument: " ^ id)
    else (Hashtbl.add seen id true; typecheck_ty l tc t)
  ) f.args;
  typecheck_ret_ty l tc f.frtyp

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier
*)

let rec create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  let with_struct_names =
    List.fold_left (fun c d ->
      match d with
      | Gtdecl ({elt=(id, fs)} as l) ->
          if lookup_struct_option id c <> None then type_error l ("Duplicate struct: " ^ id)
          else add_struct c id fs
      | _ -> c
    ) empty p
  in
  List.iter (fun d ->
    match d with
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl with_struct_names id fs l
    | _ -> ()
  ) p;
  with_struct_names

let rec create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let tc_with_builtins =
    List.fold_left (fun c (name, (args, rty)) -> add_global c name (TRef (RFun (args, rty)))) tc builtins
  in
  List.fold_left (fun c d ->
    match d with
    | Gfdecl ({elt=f} as l) ->
        if lookup_global_option f.fname c <> None then type_error l ("Duplicate function: " ^ f.fname)
        else add_global c f.fname (TRef (RFun (List.map fst f.args, f.frtyp)))
    | _ -> c
  ) tc_with_builtins p



  
let rec create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  tc


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
