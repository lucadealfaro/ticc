(** typecheck.ml *)

(** This file contains type-checking functions for Ticc. *)

open Ast 

exception TypeError
exception InternalError

(** This is the type to do type checking *)
type etype = BoolT | IntT 

(** This is the type of a list of terms, used for avoiding negative
    intermediate results. Pos, Neg keeps track of whether it must be
    added or subtracted. E stands for expression, C for constant*)
type term_t = PosE of Ast.t | NegE of Ast.t 
	      | PosC of int | NegC of int 


(** This is the function that does the type checking between the
    integer and boolean types *)
let rec typechk_expr (e: Ast.t) : etype = 
  match e with 
    Variable (v, _, _) -> 
      begin
	match (Var.get_type v) with 
	  Var.Bool -> BoolT
	| Var.Range (_,_) | Var.Clock (_) -> IntT 
      end
  | Tval (_,_) -> BoolT
  | Int (_,_) -> IntT
  | Unop (op,e,pos) -> 
      begin
	let et = typechk_expr e in 
	match op with 
	  Not -> 
	    if et = BoolT then BoolT
	    else begin
	      Ast.print_pos pos; 
	      Printf.printf " Implicit cast (due to logical 'not' operator) from int to bool not allowed.\n";
	      raise TypeError
	    end
	| Nondet -> BoolT
      end
  | Binop (op,e1,e2,pos) -> 
      let e1t = typechk_expr e1 in 
      let e2t = typechk_expr e2 in 
      begin
	match op with 
	  Gt | Ge | Lt | Le -> 
	    (* Relational operators expect integers *)
	    if e1t = IntT && e2t = IntT 
	    then BoolT
	    else begin
	      Ast.print_pos pos; 
	      Printf.printf " Implicit cast (due to relational operator) from bool to int not allowed.\n";
	      raise TypeError
	    end
	| Eq | Ne -> 
	    (* These are polymorphic: they just ask that both 
	       expressions are both IntT or both BoolT *)
	    if e1t = e2t 
	    then BoolT
	    else begin
	      Ast.print_pos pos; 
	      Printf.printf " Equal or Different operator between operands of different type.\n";
	      raise TypeError
	    end
	| And | Or | Implies (* | Equiv *) -> 
	    (* Boolean operators take boolean values *)
	    if e1t = BoolT && e2t = BoolT 
	    then BoolT
	    else begin 
	      Ast.print_pos pos; 
	      Printf.printf " Implicit cast (due to logical operator) from int to bool not allowed.\n";
	      raise TypeError
	    end
	      (* Plus and minus cast their arguments, so anything goes. *)
	| Plus | Minus -> IntT
	    (* Mod takes only integers *)
	| Mod -> 
	    if e1t = IntT && e2t = IntT 
	    then begin 
	      match e2 with 
		Int (c, _) -> IntT
	      | _ -> 
		  Ast.print_pos pos; 
		  Printf.printf " The 'mod' operator must have a constant on the rhs.\n";
		  raise TypeError
	    end
	    else begin
	      Ast.print_pos pos; 
	      Printf.printf " Implicit cast (due to 'mod' operator) from bool to int not allowed.\n";
	      raise TypeError
	    end
      end
;;

	
(** This function walks over an AST, and does two things: 
    
    1) Checks that clocks are used in the correct way: only
    comparisons to constants are allowed. 
    
    2) For each clock, updates the bound to which it is compared.
    
    The function raises the exception TypeError, and prints an error
    message, if a clock is used in an improper way.   *)

let rec update_clock_bounds_expr e = 
  let is_clock_comparison l r pos = 
    (* This is the function that checks whether a relop is actually 
       a clock comparison. *)
    match l with 
      Variable (v, primed, pos) -> 
	begin
	  match (Var.get_type v) with 
	    Var.Clock (b) -> 
	      begin
		match r with 
		  Int (n, posp) -> 
		    (* Ok, this is a clock comparison. 
		       Updates the clock bound. *) 
		    b.Var.maxval <- max n b.Var.maxval; 
		    (* yes, found a clock comparison *) 
		    true 
		| _ -> false
	      end
	  | _ -> false 
	end
    | _ -> false
  in
  match e with 
    Int (_,_) -> ()
  | Tval (_,_) -> ()
  | Unop (Not,a,_) -> update_clock_bounds_expr a
  | Unop (Nondet,a,_) -> ()
  | Binop (Eq,l,r,pos)
  | Binop (Ne,l,r,pos)
  | Binop (Gt,l,r,pos)
  | Binop (Lt,l,r,pos)
  | Binop (Ge,l,r,pos)
  | Binop (Le,l,r,pos) ->
      (* Checks by any chance if this is a clock comparison *)
      if (is_clock_comparison l r pos) then ()
      else if (is_clock_comparison r l pos) then ()
	(* else, recurs *)
      else begin
	update_clock_bounds_expr l; 
	update_clock_bounds_expr r
      end
  | Binop (_,l,r,_) -> 
      update_clock_bounds_expr l; 
      update_clock_bounds_expr r
  | Variable (v, primed, pos) -> 
      begin
	(* clock variables are not allowed outside clock comparisons *) 
	match (Var.get_type v) with
	  Var.Clock (_) -> 
	    Ast.print_pos pos; 
	    Printf.printf " Using clock %s outside a comparison with an integer constant\n" v.Var.name; 
	    raise TypeError
	| Var.Bool | Var.Range(_,_) -> ()
      end
;;

(** This function optimizes an expression, carrying out constant
    preevaluation, and also avoiding negative intermediate results in
    arithmetic operations by rearranging expressions so that one first
    sums, then subtracts.  This latter part is important, due to the
    difficulties of dealing with negative numbers.  The only case when
    it fails is if one multiplies two negative numbers. *)
let optimize_expression (e: Ast.t) : Ast.t =

  (* Gather the postive and negative terms in a summation *)
  let rec gather_terms (e: Ast.t): term_t list = 
    match e with
      Variable (_,_,_) -> [PosE e]
    | Tval (true, _) ->[PosC 1]
    | Tval (false,_) ->[PosC 0]
    | Int (c,_) -> [PosC c]
    | Unop (op,e',pos) -> 
	let (e'', t') = optimize e' in 
	[PosE (Unop (op, e'', pos))]
    | Binop (Plus, e1, e2, _) -> (gather_terms e1) @ (gather_terms e2)
    | Binop (Minus, e1, e2, _) ->
	let reverse_sign = function 
	    PosE e -> NegE e 
	  | NegE e -> PosE e 
	  | PosC c -> NegC c
	  | NegC c -> PosC c
	in
	(gather_terms e1) @ (List.map reverse_sign (gather_terms e2))
    | Binop (op,e1,e2,pos) -> 
	let (e1', t1') = optimize e1 in 
	let (e2', t2') = optimize e2 in 
	[PosE (Binop (op, e1', e2', pos))]

  (* This function optimizes the evaluation of an expression *) 
  and optimize (e: Ast.t): (Ast.t * etype) = 
    match e with 
      Variable (v,_,_) -> 
	let vt =
	  match (Var.get_type v) with 
	    Var.Bool -> BoolT
	  | _ -> IntT 
	in (e, vt)
    | Tval (_,_) -> (e, BoolT)
    | Int (_,_) -> (e, IntT)
    | Unop (Not, e', upos) ->
	begin
	  let (eopt, topt) = optimize e' in 
	  match e' with 
	    Tval (true,  pos) -> (Tval (false, pos), BoolT)
	  | Tval (false, pos) -> (Tval (true , pos), BoolT)
	  | _ -> (Unop (Not, eopt, upos), BoolT)
	end
    | Unop (Nondet, e', upos) -> (e, BoolT)
    | Binop (op, e1, e2, bpos) ->
	let (e1o, e1t) = optimize e1 in 
	let (e2o, e2t) = optimize e2 in 
	begin
	  match op with 
	    Gt | Lt | Ge | Le -> (Binop (op, e1o, e2o, bpos), BoolT)
	  | And -> 
	      begin
		match (e1o, e2o) with 
		  (Tval (false, ps), _)
		| (_, Tval (false, ps)) -> (Tval (false, ps), BoolT)
		| (Tval (true, _), eopt)
		| (eopt, Tval (true, _)) -> (eopt, BoolT)
		| (_,_) -> (Binop (And, e1o, e2o, bpos), BoolT)
	      end
	  | Or -> 
	      begin
		match (e1o, e2o) with 
		  (Tval (true, ps), _)
		| (_, Tval (true, ps)) -> (Tval (false, ps), BoolT)
		| (Tval (false, _), eopt)
		| (eopt, Tval (false, _)) -> (eopt, BoolT)
		| (_,_) -> (Binop (Or, e1o, e2o, bpos), BoolT)
	      end
	  | Implies -> 
	      begin
		match (e1o, e2o) with 
		  (Tval (false, ps), _)
		| (_, Tval (true, ps)) -> (Tval (false, ps), BoolT)
		| (Tval (true, _), eopt) -> (eopt, BoolT)
		| (eopt, Tval (false, _)) -> (Unop (Not, eopt, bpos), BoolT)
		| (_,_) -> (Binop (Implies, e1o, e2o, bpos), BoolT)
	      end
	  | Eq -> 
	      (* Equality can be of two types: bool, and int. *)
	      if e1t = BoolT 
	      then begin
		(* Classical simplification of equivalence *)
		match (e1o, e2o) with 
		  (Tval (false, ps), Tval (false, ps'))
		| (Tval (true , ps), Tval (true , ps')) -> (Tval (true , ps), BoolT)
		| (Tval (false, ps), Tval (true , ps'))
		| (Tval (true , ps), Tval (false, ps')) -> (Tval (false, ps), BoolT)
		| (Tval (true , ps), eopt) -> (eopt, BoolT)
		| (eopt, Tval (true , ps)) -> (eopt, BoolT)
		| (Tval (false, ps), eopt) -> (Unop (Not, eopt, bpos), BoolT)
		| (eopt, Tval (false, ps)) -> (Unop (Not, eopt, bpos), BoolT)
		| (_,_) -> (Binop (Eq, e1o, e2o, bpos), BoolT)
	      end else 
		(* Equality of integers: simplify separately *)
		(Binop (Eq, e1o, e2o, bpos), IntT)
	  | Ne -> 
	      if e1t = BoolT 
	      then begin
		(* Classical simplification of not equivalence *)
		match (e1o, e2o) with 
		  (Tval (false, ps), Tval (false, ps'))
		| (Tval (true , ps), Tval (true , ps')) -> (Tval (false, ps), BoolT)
		| (Tval (false, ps), Tval (true , ps'))
		| (Tval (true , ps), Tval (false, ps')) -> (Tval (true, ps), BoolT)
		| (Tval (true , ps), eopt) -> (Unop (Not, eopt, bpos), BoolT)
		| (eopt, Tval (true , ps)) -> (Unop (Not, eopt, bpos), BoolT)
		| (Tval (false, ps), eopt) -> (eopt, BoolT)
		| (eopt, Tval (false, ps)) -> (eopt, BoolT)
		| (_,_) -> (Binop (Ne, e1o, e2o, bpos), BoolT)
	      end else
		(* Inequality of integers: simplify separately *)
		(Binop (Ne, e1o, e2o, bpos), IntT)
	  | Plus | Minus -> 
	      let terms = gather_terms e in 
	      (* sorts the terms according to their type: positive,
		 negative, or constants *)
	      let sort_terms (pe, cl, ne) = function 
		  PosE t -> ((PosE t) :: pe, cl, ne)
		| NegE t -> (pe, cl, (NegE t) :: ne)
		| PosC c -> (pe, c :: cl, ne)
		| NegC c -> (pe, -c :: cl, ne)
	      in 
	      let (pel, cl, nel) = List.fold_left sort_terms ([], [], []) terms  in 
	      (* Adds up the constants *)
	      let curryplus x y = x + y in 
	      let sumc    = List.fold_left curryplus 0 cl in 
	      (* This function combines a list of expressions into a single expression. *)
	      let rec add_terms = function 
		  [] -> Int (0, bpos)
		| [PosE t] -> t 
		| [NegE t] -> Binop (Minus, Int (0, bpos), t, bpos)
		| [PosC c] -> Int (c, bpos)
		| [NegC c] -> Binop (Minus, Int (0, bpos), Int (c, bpos), bpos)
		| (PosE t) :: rl -> Binop (Plus,  (add_terms rl), t, bpos)
		| (NegE t) :: rl -> Binop (Minus, (add_terms rl), t, bpos)
		| (PosC c) :: rl -> Binop (Plus,  (add_terms rl), Int (c, bpos), bpos)
		| (NegC c) :: rl -> Binop (Minus,  (add_terms rl), Int (c, bpos), bpos)
	      in

	      (* Produces the list of all positive and negative terms,
		 and the constants ... *)
	      let rev_add_list = 
		if sumc < 0 
		then pel @ [NegC (-sumc)] @ nel 
		else if sumc > 0
		then pel @ [PosC sumc] @ nel 
		else pel @ nel 
	      in 
	      
	      let new_term = add_terms (List.rev rev_add_list) in 
	      (new_term, IntT)

	  | Mod -> (Binop (Mod, e1o, e2o, bpos), IntT)
	end
  in
  let (new_e, new_t) = optimize e in new_e
	
(** This function type-checks an expression, and check that the type
    of the expression is boolean.  It also computes clock bounds for it. *)
let type_check_bool (e: Ast.t) : unit =
  if (typechk_expr e) = IntT 
  then begin
    Ast.print_epos e; 
    Printf.printf " The top-level expression here is not of boolean type.\n";
    raise TypeError
  end 


(** this function checks that no primed vars appear in the guard *)
let check_guard_no_primed g = 
  let chkg v pos = 
    Ast.print_pos pos; 
    Printf.printf " Error: primed variable %s' used in guard\n" (Var.get_name v);
    raise TypeError
  in 
  iter_primed_vars g chkg


(** Checks a guarded command: guard and command must be bool, and 
    the clock bounds need to be updated.  
    As input, it takes the list of guarded commands. 
    It returns (lr, lw), the lists of read and written variables, 
    possibly with repetitions. *) 
let check_guarded_command_list (gcl: (Ast.t * Ast.t) list) : ((Var.t list) * (Var.t list)) = 
  (* now collects the read and written vaiables, with repetitions *) 
  let check_gc (lr, lw) (g, c) = 
    type_check_bool g; 
    check_guard_no_primed g; 
    update_clock_bounds_expr g; 
    type_check_bool c; 
    update_clock_bounds_expr c; 
    let lr' = (get_unprimed_vars g) @ (get_unprimed_vars c) @ lr in  
    let lw' = (get_primed_vars g)   @ (get_primed_vars c)   @ lw in  
    (lr', lw')
  in List.fold_left check_gc ([], []) gcl


(** Checks an output rule: guards and command have to be boolean. 
    Also, collects and sets the lists of read and written variables by
    the rule, and the list of module variables. *) 
let check_orule (m: Mod.t) (r: Rule.orule_t) : unit =
  (* function to check a single guarded command *)
  (* lr and lw are the lists of read and written variables of the gc, possibly with repetitions. *) 
  let (lr, lw) = check_guarded_command_list (Rule.get_ogc_list r) in 
  List.iter (Rule.add_or_var r) lr; 
  (* written variables must also be read *) 
  List.iter (Rule.add_or_var r) lw; 
  List.iter (Rule.add_ow_var r) lw; 
  (* updated module variables *) 
  List.iter (Mod.add_var m) lr; 
  List.iter (Mod.add_var m) lw


(** Checks that a local rule changes only local variables *) 
let check_lrule (m: Mod.t) (r: Rule.lrule_t) : unit =
  (* function to check a single guarded command *)
  (* lr and lw are the lists of read and written variables of the gc, possibly with repetitions. *) 
  let (lr, lw) = check_guarded_command_list (Rule.get_lgc_list r) in 
  List.iter (Rule.add_lr_var r) lr; 
  (* written variables must also be read *) 
  List.iter (Rule.add_lr_var r) lw; 
  List.iter (Rule.add_lw_var r) lw; 
  (* updated module variables *) 
  List.iter (Mod.add_var m) lr; 
  List.iter (Mod.add_var m) lw; 
  (* function to check a single guarded command, checking
     that the only primed vars that 
     appear in guard, and in the command, are local *)
  let check_gc (g, c) = 
    (* this function checks a single variable *)
    let chkv v pos = 
      (* local vars are ok *) 
      if not (Mod.is_lvar_name_def m (Var.get_name v))
      then begin
	Ast.print_pos pos; 
	Printf.printf " Error: %s' not local in local action %s\n"
	  (Var.get_name v) (Rule.get_lname r); 
	raise TypeError
      end
    in 
    iter_primed_vars c chkv; 
    iter_primed_vars g chkv
  in
  List.iter check_gc (Rule.get_lgc_list r)

      
(** 
    Checks an input rule: 
    (1) Checks that the local assignments are listed in 
    the order of the implicit dependency relation.
    That is, the assignment v' = e can contain a primed variable x'
    in e only if either x is global, or if there was a preceding a
    ssignment x' = f for some f.  This ensures determinacy. *)
let check_irule (m: Mod.t) (r: Rule.irule_t) =
  (* Function to check a global guarded command. 
     Only global variables (no local ones) can be modified,
     and they have to be declared among the effects of the action. *)
  (* this function checks a global command *) 
  let (lr, lw) = check_guarded_command_list (Rule.get_global_igc_list r) in 
  List.iter (Rule.add_ir_var r) lr; 
  (* written variables must also be read *) 
  List.iter (Rule.add_ir_var r) lw; 
  List.iter (Rule.add_iwg_var r) lw; 
  (* update module variables *) 
  List.iter (Mod.add_var m) lr; 
  List.iter (Mod.add_var m) lw;
  
  (* This function checks that all primed variables of the rhs of an 
     assignment are either global, or appear in nextvars. *)
  let check_rhs_asgn nextvars e = 
    let chkc v pos = 
      if (List.mem v nextvars) || (Var.is_global v)
      then ()
      else begin
	Ast.print_pos pos; 
	Printf.printf " Error: the variable %s' is not assigned in a preceding command, nor it is global\n" 
	  (Var.get_name v); 
	raise TypeError
      end in 
    iter_primed_vars e chkc
  in
  (* This function checks an assignment, checking that: 
     1) The assigned var is not in nextvars
     2) the expression passes the check_rhs_asgn check.
     It returns the variable on the lhs, for convenience. 
     It also updates the list of read and written vars of the rule *)
  let check_asgn nextvars e =
    type_check_bool e; 
    update_clock_bounds_expr e; 
    match e with 
      Binop (Eq, Variable (v, true, vpos), rhs, pos) -> 
	(* first, checks the lhs variable *) 
	(* it cannot appear in the list *) 
	if List.mem v nextvars 
	then begin
	  Ast.print_pos pos; 
	  Printf.printf " Error: %s' is already assigned in a preceding command\n"
	    (Var.get_name v); 
	  raise TypeError
	end
	else begin
	  (* checks that it is a local variable *) 
	  if not (Mod.is_lvar_name_def m (Var.get_name v))
	  then begin
	    Ast.print_pos pos; 
	    Printf.printf " Error: %s' is not a local variable in the local portion of an input action\n"
	      (Var.get_name v); 
	    raise TypeError
	  end
	end;
	(* second, checks the rhs expression *) 
	check_rhs_asgn nextvars rhs; 
	(* now we need to add to the rule the list of global variables it reads *)
	let u_read_vars = get_unprimed_vars rhs in 
	let p_read_vars = get_primed_vars rhs in 
	List.iter (Rule.add_ir_var r) u_read_vars; 
	List.iter (Rule.add_ir_var r) p_read_vars; 
	(* adds the variables to the module *) 
	List.iter (Mod.add_var m) u_read_vars; 
	List.iter (Mod.add_var m) p_read_vars; 
	(* no need to add the lhs variable to the module, as it is local *) 
	(* adds the lhs variable to the list of variables that are written *)
	Rule.add_iwl_var r v; 
	(* returns the variable *) 
	v
    | Binop (_,_,_,pos) 
    | Unop (_,_,pos)
    | Int (_,pos)
    | Tval (_,pos)
    | Variable (_,_,pos) -> 
	(* something is wrong: this should be an assignment. *)
	print_string "\n"; 
	Ast.print_pos pos; 
	print_string " Something is wrong: "; 
	Ast.print e;
	print_string "should be an assignment!\n";
	raise InternalError
  in
  (* This function checks a list of assignments *) 
  let rec check_asgn_list nextvars = function 
      [] -> ()
    | asgn :: asgnl -> 
	(* checks the assignment at the head of the list *)
	let v = check_asgn nextvars asgn in
	(* checks the other assignments, with v added to the list 
	   of allowed vars *) 
	check_asgn_list (v::nextvars) asgnl
  in
  (* This function checks a guard asgn_list pair *)
  let check_local_part (g, asgnl) = 
    (* checks the guard *)
    type_check_bool g; 
    update_clock_bounds_expr g;
    (* checks that the primed variables in the guard are all global *)
    (* this function checks a single variable *)
    let chkv v pos = 
      (* local vars are not ok *) 
      if (Mod.is_lvar_name_def m (Var.get_name v))
      then begin
	Ast.print_pos pos; 
	Printf.printf " Error: primed local variables (%s') are forbidden in the guard of the local part of an input action (%s)\n"
	  (Var.get_name v) (Rule.get_iname r); 
	raise TypeError
      end
    in 
    iter_primed_vars g chkv; 
    (* updated module variables *)
    let vars = get_vars g in
    List.iter (Mod.add_var m) vars;
    (* checks the command *)
    check_asgn_list [] asgnl
  in
  (* Finally, we check the local part of the command. Pfew. *)
  List.iter check_local_part (Rule.get_local_igc_list r)


(** This function checks that there are no primed variables in an
    invariant *)
(* Luca: removed the requirement that the invariant only mentions the history-full
   variables in hful *)
let check_invariant (m: Mod.t) (e : Ast.t) : unit = 
  let hful = Mod.get_hvars m in 
    (* Checks no primed variables *)
  let chkg v pos = 
    Ast.print_pos pos; 
    Printf.printf " Error: %s': primed variables cannot appear in invariant.\n" (Var.get_name v);
    raise TypeError
  in 
  iter_primed_vars e chkg; 
  (* Checks no history-free variables *)
  (* Luca: this requirement has been commented out. I think it is not correct. 
  let chkg v pos = 
    let v_name = Var.get_name v in 
    if not (Hsetmap.mem (Mod.get_hvars m) v_name) then begin
      Ast.print_pos pos; 
      Printf.printf " Error: %s: history-free variables cannot appear in invariant.\n" v_name;
      raise TypeError
    end 
  in iter_unprimed_vars e chkg; 
  *)
  (* does some final type checking *) 
  type_check_bool e;
  update_clock_bounds_expr e;
  (* adds variables to the module *)
  let vars = get_unprimed_vars e in
  List.iter (Mod.add_var m) vars 

(** This function checks that there are no primed variables in an
    initial condition.  It used to check that all variables appearing
    in the initial condition are local, but it no longer does that. *)
let check_init (m: Mod.t) (e : Ast.t) : unit = 
  let loc_vars = Mod.get_hvars m in 
  (* check no primed variables *)
  let chkg v pos = 
    Ast.print_pos pos; 
    Printf.printf " Error: %s': primed variables cannot appear in invariants.\n" (Var.get_name v);
    raise TypeError
  in 
  iter_primed_vars e chkg; 
  (* Luca: removed: Checks that all variables are local *)
  (* Luca: removed
  let chkg v pos = 
    let v_name = Var.get_name v in 
    if not (Hsetmap.mem (Mod.get_lvars m) v_name) then begin
      Ast.print_pos pos; 
      Printf.printf " Error: %s: the initial condition must cite only local variables.\n" v_name;
      raise TypeError
    end 
  in iter_unprimed_vars e chkg; 
  *)   
  (* does some final type checking *) 
  type_check_bool e;
  update_clock_bounds_expr e;
  (* adds variables to the module *)
  let vars = get_unprimed_vars e in
  List.iter (Mod.add_var m) vars 

(** This function checks that a module follows all the rules *)
let check_module (m: Mod.t) : unit = 
  (* checks the rules *) 
  Hsetmap.iter_body (check_lrule m) (Mod.get_lrules m); 
  Hsetmap.iter_body (check_orule m) (Mod.get_orules m); 
  Hsetmap.iter_body (check_irule m) (Mod.get_irules m);
  (* Checks that the initial condition contains only local variables *)
  List.iter (check_init m) (Mod.get_init m); 
  (* at this point, the vars of the module are known. *) 
  (* computes the history-full variables as the difference between 
     the variables of the module and the free variables. *) 
  Mod.set_hvars m;
  (* checks that invariants only mention history-full variables *) 
  Hsetmap.iter_body (check_invariant m) (Mod.get_ssets m); 
  List.iter (check_invariant m) (Mod.get_iinv m);
  List.iter (check_invariant m) (Mod.get_oinv m)


(** This functon optimizes a module *)
let optimize_module (m: Mod.t) : unit = 
  (* Optimizes invariants *)
  let new_iinv = List.map optimize_expression (Mod.get_iinv m) in 
  Mod.set_iinv m new_iinv; 
  let new_oinv = List.map optimize_expression (Mod.get_oinv m) in 
  Mod.set_oinv m new_oinv; 
  (* Optimizes the initial condition *)
  let new_init = List.map optimize_expression (Mod.get_init m) in 
  Mod.set_init m new_init; 

  (* Optimizes the statesets *) 
  let optimize_set (h: (string, Ast.t) Hsetmap.t) (n: string) (e: Ast.t) : unit = 
    let new_e = optimize_expression e in 
    Hsetmap.add h n new_e
  in
  let new_h : (string, Ast.t) Hsetmap.t = Hsetmap.mk () in 
  Hsetmap.iter (optimize_set new_h) (Mod.get_ssets m); 
  Mod.set_ssets m new_h; 

  (* How to optimize a guarded command (except input local) *)
  let optimize_gc (g, c) = 
    let g' = optimize_expression g in 
    let c' = optimize_expression c in 
    (g', c')
  in

  (* Optimizes an output rule *)
  let opt_orule (r: Rule.orule_t) = 
    let gcl      = Rule.get_ogc_list r in 
    let new_gcl  = List.map optimize_gc gcl in
    Rule.set_ogc_list r new_gcl
  in

  (* Optimizes a local rule *)
  let opt_lrule (r: Rule.lrule_t) = 
    let gcl      = Rule.get_lgc_list r in 
    let new_gcl  = List.map optimize_gc gcl in 
    Rule.set_lgc_list r new_gcl
  in

  (* Optimizes an input rule *)
  let opt_irule (r: Rule.irule_t) = 
    (* Global portion *)
    let gcl = Rule.get_global_igc_list r in 
    let new_gcl = List.map optimize_gc gcl in 
    (* replace the old rule with the new one *)
    Rule.set_global_igc_list r new_gcl;

    (* Local portion *)
    let lgcl = Rule.get_local_igc_list r in 
    let optimize_lirule (g, cl) = 
      let new_g = optimize_expression g in 
      let optimize_asg asg = 
	match asg with 
	  Binop (Eq, Variable (v,p,posv), e, pos) -> 
	    Binop (Eq, Variable (v,p,posv), (optimize_expression e), pos)
	| e -> e
      in 
      let new_cl = List.map optimize_asg cl in 
      (new_g, new_cl)
    in
    let new_lgcl = List.map optimize_lirule lgcl in 
    (* replace the old rule with the new one *)
    Rule.set_local_igc_list r new_lgcl
  in

  (* Now first gets the lists of commands, then optimizes them. 
     Note: you cannot use Hsetmap.iter_body, as the optimization
     functions (unlike type checking) can change the rules. *)
  let orule_l = Hsetmap.copy (Mod.get_orules m) in 
  let lrule_l = Hsetmap.copy (Mod.get_lrules m) in 
  let irule_l = Hsetmap.copy (Mod.get_irules m) in 
  Hsetmap.iter_body opt_orule orule_l; 
  Hsetmap.iter_body opt_lrule lrule_l;
  Hsetmap.iter_body opt_irule irule_l 

