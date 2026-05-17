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
(* Helper function that gives the underlying reference type of the nullable type*)
let typ_of_nullable = function
  | TNullRef r -> Some (TRef r)
  | _ -> None


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
  match e.elt with
  | CInt _ -> TInt
  | CBool _ -> TBool
  | CNull r -> TNullRef r
  | CStr _ -> TRef RString
  | Lhs l -> fst (typecheck_lhs c l)
  | CStruct (id, fs) ->
      begin match lookup_struct_option id c with
      | None -> type_error e ("Unknown struct: " ^ id)
      | Some decl_fs ->
          List.iter (fun (fname, fexp) ->
            match List.find_opt (fun f -> f.fieldName = fname) decl_fs with
            | None -> type_error e ("Unknown field: " ^ fname)
            | Some fdecl ->
                let te = typecheck_exp c fexp in
                if not (subtype c te fdecl.ftyp) then type_error e "Bad struct field type"
          ) fs;
          TRef (RStruct id)
      end
  | Call (fexp, args) ->
      begin match typecheck_exp c fexp with
      | TRef (RFun (param_tys, rt))
      | TNullRef (RFun (param_tys, rt)) ->
          if List.length args <> List.length param_tys then type_error e "Wrong number of args";
          List.iter2 (fun a pty ->
            let aty = typecheck_exp c a in
            if not (subtype c aty pty) then type_error e "Bad call arg type"
          ) args param_tys;
          begin match rt with RetVoid -> type_error e "Void function used as expression" | RetVal t -> t end
      | _ -> type_error e "Call target is not a function"
      end
    | Bop (Eq, e1, e2)
    | Bop (Neq, e1, e2) ->
        let t1 = typecheck_exp c e1 in
        let t2 = typecheck_exp c e2 in
        if subtype c t1 t2 || subtype c t2 t1 then TBool
        else type_error e "==/!= operands are not compatible"
    | Bop (b, e1, e2) ->
        let t1_expected, t2_expected, tret = typ_of_binop b in
        let t1 = typecheck_exp c e1 in
        let t2 = typecheck_exp c e2 in
        if not (subtype c t1 t1_expected) then type_error e "left operand has wrong type";
        if not (subtype c t2 t2_expected) then type_error e "right operand has wrong type";
        tret
    | Uop (u, e1) ->
        let t_expected, t_ret = typ_of_unop u in
        let t = typecheck_exp c e1 in
        if subtype c t t_expected then t_ret else type_error e "unary operand has wrong type"
    | CArr (elt_t, es) ->
        typecheck_ty e c elt_t;
        List.iter (fun ei ->
          let ti = typecheck_exp c ei in
          if not (subtype c ti elt_t) then type_error ei "array element has wrong type"
        ) es;
        TRef (RArray elt_t)
    | NewArr (elt_t, size_e) ->
        typecheck_ty e c elt_t;
        if typecheck_exp c size_e <> TInt then type_error size_e "array size must be int";
        if is_nullable_ty elt_t || elt_t = TInt || elt_t = TBool then TRef (RArray elt_t)
        else type_error e "this array type needs explicit initializer"
    | NewArrInit (elt_t, size_e, idx, body_e) ->
        typecheck_ty e c elt_t;
        if typecheck_exp c size_e <> TInt then type_error size_e "array size must be int";
        let c_with_idx = add_local c idx TInt in
        let body_t = typecheck_exp c_with_idx body_e in
        if not (subtype c body_t elt_t) then type_error e "array initializer has wrong type";
        TRef (RArray elt_t)
    | Length e1 ->
        begin match typecheck_exp c e1 with
        | TRef (RArray _) | TNullRef (RArray _) -> TInt
        | _ -> type_error e "length expects an array"
        end

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
  match l.elt with
  | Id id ->
      begin match lookup_local_option id c with
      | Some t -> (t, true)
      | None ->
          begin match lookup_global_option id c with
          | Some (TRef (RFun _ ) as t) -> (t, false)
          | Some t -> (t, true)
          | None -> type_error l ("Unbound id: " ^ id)
          end
      end
  | Proj (e, fname) ->
      begin match typecheck_exp c e with
      | TRef (RStruct sid)
      | TNullRef (RStruct sid) ->
          begin match lookup_field_option sid fname c with
          | Some t -> (t, true)
          | None -> type_error l ("Unknown field: " ^ fname)
          end
      | _ -> type_error l "Projection on non-struct"
      end
  | Index (arr, idx) ->
      let it = typecheck_exp c idx in
      if it <> TInt then type_error l "Array index must be int";
      begin match typecheck_exp c arr with
      | TRef (RArray t)
      | TNullRef (RArray t) -> (t, true)
      | _ -> type_error l "Index on non-array"
      end

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
  match s.elt with
  | Assn (lhs, rhs) ->
      let (lt, assignable) = typecheck_lhs tc lhs in
      if not assignable then type_error s "assignment to non-assignable lhs";
      let rt = typecheck_exp tc rhs in
      if not (subtype tc rt lt) then type_error s "assignment type mismatch";
      (tc, false)
  | Decl (id, e) ->
      let t = typecheck_exp tc e in
      (add_local tc id t, false)
  | Ret None ->
      begin match to_ret with
      | RetVoid -> (tc, true)
      | RetVal _ -> type_error s "missing return value"
      end
  | Ret (Some e) ->
      let t = typecheck_exp tc e in
      begin match to_ret with
      | RetVoid -> type_error s "void function returning value"
      | RetVal tr -> if not (subtype tc t tr) then type_error s "bad return type" else (tc, true)
      end
  | SCall (fexp, args) ->
      begin match typecheck_exp tc fexp with
      | TRef (RFun (param_tys, RetVoid))
      | TNullRef (RFun (param_tys, RetVoid)) ->
          if List.length args <> List.length param_tys then type_error s "Wrong number of args";
          List.iter2 (fun a pty -> if not (subtype tc (typecheck_exp tc a) pty) then type_error s "Bad call arg type") args param_tys;
          (tc, false)
      | TRef (RFun _) | TNullRef (RFun _) -> type_error s "non-void function used as statement call"
      | _ -> type_error s "statement call target is not a function"
      end
    | If (g, b1, b2) ->
      if typecheck_exp tc g <> TBool then type_error g "if guard must be bool";
      let _, r1 = typecheck_block tc b1 to_ret in
      let _, r2 = typecheck_block tc b2 to_ret in
      (tc, r1 && r2)
  | While (g, body) ->
      if typecheck_exp tc g <> TBool then type_error g "while guard must be bool";
      let _, _ = typecheck_block tc body to_ret in
      (tc, false)
  | For (inits, guard, after, body) ->
      let c_with_inits =
        List.fold_left (fun c_acc (id, e) ->
          let t = typecheck_exp c_acc e in
          add_local c_acc id t
        ) tc inits
      in
      begin match guard with
      | None -> ()
      | Some g when typecheck_exp c_with_inits g = TBool -> ()
      | Some g -> type_error g "for guard must be bool"
      end;
      begin match after with
      | None -> ()
      | Some s_after -> ignore (typecheck_stmt c_with_inits s_after to_ret)
      end;
      let _, _ = typecheck_block c_with_inits body to_ret in
      (tc, false)
  | Cast (rty, id, exp, notnull_b, null_b) ->
      let t_exp = typecheck_exp tc exp in
      begin match t_exp with
      | TNullRef r when subtype_ref tc r rty || subtype_ref tc rty r ->
          let tc_notnull = add_local tc id (TRef rty) in
          let _, r1 = typecheck_block tc_notnull notnull_b to_ret in
          let _, r2 = typecheck_block tc null_b to_ret in
          (tc, r1 && r2)
      | _ -> type_error s "if? expression must be a nullable reference"
      end

and typecheck_block (tc : Tctxt.t) (b : Ast.block) (to_ret : ret_ty) : Tctxt.t * bool =
  List.fold_left (fun (c_acc, definitely_returns) st ->
    let c_next, returns_here = typecheck_stmt c_acc st to_ret in
    (c_next, definitely_returns || returns_here)
  ) (tc, false) b


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
  let tc_with_args =
    List.fold_left (fun c (t, id) ->
      if Hashtbl.mem seen id then type_error l ("Duplicate argument: " ^ id)
      else (Hashtbl.add seen id true; typecheck_ty l tc t; add_local c id t)
    ) tc f.args
  in
  typecheck_ret_ty l tc f.frtyp;
  let _, definitely_returns =
    List.fold_left (fun (c, retflag) st ->
      let c2, r = typecheck_stmt c st f.frtyp in
      (c2, retflag || r)
    ) (tc_with_args, false) f.body
  in
  match f.frtyp with
  | RetVoid -> ()
  | RetVal _ -> if not definitely_returns then type_error l ("Function " ^ f.fname ^ " might not return")

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
  List.fold_left (fun c d ->
    match d with
    | Gvdecl ({elt=g} as l) ->
        if lookup_global_option g.name c <> None then type_error l ("Duplicate global: " ^ g.name)
        else
          let t = typecheck_exp c g.init in
          add_global c g.name t
    | _ -> c
  ) tc p


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
