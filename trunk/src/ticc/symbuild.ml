(** symbuild.ml *)

open Ast

(** This is where we build the symbolic representation 
*)

module VarSet = Vset.VS

type varid_t = Vset.varid_t 
type stateset_t = Mlglu.mdd

exception SymbolicTypeError
exception InternalError

let debug = false

(** Type of a symbolic representation computed from the Ast. 
    The recursive function that computes MDDs from an Ast returns
    objects of this type.  If the Ast is boolean, it will return a
    RelMdd (m), for an MDD m.  The type checker ensures that this
    is the case. *)

type predd = 
    (* This represents a single variable (e.g., a leaf). 
       I carry the whole variable, as I will need to know also the
       type. *)
    Varid of varid_t * Var.data_t 
      (* This represents an MDD that already encodes a truth-set. *)
  | RelMdd of Mlglu.mdd 
      (* This represents an MDD for x=f, together with the variable x *)
  | MultiMdd of varid_t * Mlglu.mdd
      (* This is just a numerical constant *)
  | Constant of int 


(** [log2 n] computes the ceiling of the base-2 logarithm of [n]. *)
let rec log2 (n: int) : int =
  if (n <= 2) then 1
  else 1 + (log2 ((n+1) asr 1))


(** Returns the maximum number of bits required by any intermediate
  result during the evaluation of expression [e]. *)
let max_nbits (e: Ast.t) : int =
  let tmp_max = ref 1 in
  
  (** It returns the number of bits needed to represent the root
    of the expression [e2]. At the same time, it updates [tmp_max]
    with the maximum # of bits required by any intermediate result. *)
  let rec max_nbits_step (e2: Ast.t) : int =
    let new_val =
      match e2 with
	(** leaves *)
	Int(n, _)         -> log2 (n + 1)
      | Tval(_,_)         -> 1
      | Variable(v, _, _) -> log2 (Var.nvals v)
	(** internal nodes *)
      | Binop(op, l, r, _) -> 
	  let n_l = max_nbits_step l
	  and n_r = max_nbits_step r in
	  begin
	    match op with
	      Eq | Ne | Gt | Lt | Ge | Le | And | Or | Implies -> 1
	    | Plus  -> 1 + (max n_l n_r)
	    | Minus -> n_l
	    | Mod   -> n_r
	  end
      | Unop (op, l, _) -> 
	  let result =
	    match op with 
	      Not -> 1
	    | Nondet -> 0
	  in
	  result
    in
    tmp_max := max !tmp_max new_val;
    new_val
  in
  ignore(max_nbits_step e);
  !tmp_max
    

(** Conjoin a list of MDDs to an MDD *) 
let conj_mdd_list mgr m ml = 
  let f b b' = Mlglu.mdd_and b b' 1 1 in 
  List.fold_left f m ml 


let build_predd (s: Symprog.t) (e: Ast.t) : predd = 

  let mgr = Symprog.get_mgr s in 
  let max_bits = max_nbits e in
  (* DEBUG *)
  (* Printf.printf " max_bits=%d \n" max_bits; *)

  (** Given an op, and two integers c and c',
    builds an mdd for:
    (c op c')  *)

  let build_c_c
    (op: Ast.binop) 
    (c : int) 
    (c': int) : predd = 
    match op with 
      Eq | Ne | Gt | Ge | Lt | Le -> 
	let truth =
	  match  op with 
	    Eq -> c == c'
	  | Ne -> c != c'
	  | Gt -> c >  c'
	  | Ge -> c >= c'
	  | Lt -> c <  c'
	  | Le -> c <= c'
	  | _  -> raise InternalError
	in
	if truth then RelMdd (Mlglu.mdd_one mgr) 
	else RelMdd (Mlglu.mdd_zero mgr) 

    | Plus  -> Constant(c + c')
    | Minus -> Constant(c - c')
    | Mod   -> Constant(c mod c')
    | And | Or | Implies -> raise SymbolicTypeError
  in


  (** Given an op, a flag, and two mdd's m and m',
    builds an mdd for:
    (m  op m') if flag = false
    (m' op m)  if flag = true *)

  let build_r_r 
    (op: Ast.binop) 
    (m : Mlglu.mdd) 
    (m': Mlglu.mdd) : predd = 

    let new_m: Mlglu.mdd = match op with
	Eq      -> Mlglu.mdd_xnor m m' 
      | Ne      -> Mlglu.mdd_xor  m m'
      | Implies -> Mlglu.mdd_or   m m' 0 1
      | And     -> Mlglu.mdd_and  m m' 1 1
      | Or      -> Mlglu.mdd_or   m m' 1 1 

      | _ -> raise SymbolicTypeError
    in 
    RelMdd (new_m)
  in


  (** Given an op, a flag, a boolean variable id, and an mdd m,
    builds an mdd for:
    (id op m) if flag = false
    (m op id) if flag = true *)

  let build_v_r 
    (op: Ast.binop) 
    (flag: bool) 
    (id: varid_t) 
    (m: Mlglu.mdd) : predd = 

    let mid  = Mlglu.mdd_literal mgr id [1] in
    let new_m =  match (op, flag) with 

	(Eq, _)          -> Mlglu.mdd_xnor mid m 
      | (Ne, _)          -> Mlglu.mdd_xor  mid m
      | (Implies, false) -> Mlglu.mdd_or   mid m 0 1
      | (Implies, true)  -> Mlglu.mdd_or   mid m 1 0
      | (And, _)         -> Mlglu.mdd_and  mid m 1 1
      | (Or, _)          -> Mlglu.mdd_or   mid m 1 1 
	  
      | _                -> raise SymbolicTypeError
    in 
    RelMdd (m)
  in


  (** Given an op, a flag, a variable id, a variable id', 
    a list idl of variable ids to quantify (sublist of [id; id']), 
    and an mdd list ml, build an mdd for: 

    \exists idl . (id op id' \wedge ml) if flag = false
    \exists idl . (id' op id \wedge ml) if flag = true *)

  let build_v_v_bool 
    (op: Ast.binop) 
    (id: varid_t) 
    (id': varid_t) : predd = 
    match op with 
      Eq | Ne | Implies -> 
	let m = match op with
	    Eq      -> Mlglu.mdd_eq_s  mgr id  id' 
	  | Ne      -> Mlglu.mdd_neq_s mgr id  id'
	  | Implies -> Mlglu.mdd_geq_s mgr id  id' 
	  | _       -> raise InternalError
	in RelMdd (m) 
    | And | Or -> 
	let mid  = Mlglu.mdd_literal mgr id  [1] in
	let mid' = Mlglu.mdd_literal mgr id' [1] in
	let m = match op with 
	    And -> Mlglu.mdd_and mid mid' 1 1
	  | Or  -> Mlglu.mdd_or  mid mid' 1 1 
	  | _   -> raise InternalError
	in RelMdd (m)
    | _ -> raise SymbolicTypeError
  in


  (** Given an op, a flag, a variable id, a variable id', 
    a list idl of variable ids to quantify (sublist of [id; id']), 
    and an mdd list ml, build an mdd for: 

    \exists idl . (id  op id' \wedge ml) if flag = false
    \exists idl . (id' op id  \wedge ml) if flag = true *)

  let build_v_v_int
    (op: Ast.binop) 
    (flag: bool) 
    (id: varid_t) 
    (id': varid_t) 
    (idl: varid_t list) 
    (ml: Mlglu.mdd list) : predd = 
    match op with 
      Eq | Ne | Gt | Ge | Lt | Le -> 
	begin
	  let m = match (op, flag) with
	      (Eq, _)     -> Mlglu.mdd_eq  mgr id  id' 
	    | (Ne, _)     -> Mlglu.mdd_neq mgr id  id'
	    | (Gt, false) -> Mlglu.mdd_gt  mgr id  id' 
	    | (Gt, true)  -> Mlglu.mdd_gt  mgr id' id
	    | (Ge, false) -> Mlglu.mdd_geq mgr id  id' 
	    | (Ge, true)  -> Mlglu.mdd_geq mgr id' id 
	    | (Lt, false) -> Mlglu.mdd_lt  mgr id  id' 
	    | (Lt, true)  -> Mlglu.mdd_lt  mgr id' id
	    | (Le, false) -> Mlglu.mdd_leq mgr id  id' 
	    | (Le, true)  -> Mlglu.mdd_leq mgr id' id 
	    | _           -> raise InternalError
	  in
	  (* conjoin [m] with each mdd in the list [ml] *)
	  let m'  = conj_mdd_list mgr m ml in 
	  (* existentially quantify over temporary variables *)
	  let m'' = Mlglu.mdd_smooth_list mgr m' idl in
	  List.iter (Symtemp.release mgr) idl; 
	  RelMdd (m'') 
	end
    | And | Or | Implies -> raise SymbolicTypeError
    | Plus | Minus | Mod -> 
	begin
	  (** Gets a temporary variable *) 
	  let id'' = Symtemp.allocate mgr max_bits in 
	  let m = match (op, flag) with 
	      (* Is Serdar's Plus really better?
		 It seems to be buggier! *) 
	      (Plus, _)      -> Mlglu.mdd_eq_plus  mgr id'' id id' 
	      (*  (Plus, _)      -> Mlglu.mdd_add_s    mgr id'' id id' *)
	    | (Minus, flag) -> begin
		let (id1, id2) = match flag with 
		    false -> (id, id')
		  | true  -> (id', id) 
		in 
		(* Now I must put in id'' the mdd for 
		   (id1 - id2) /\ (id1 >= id2), 
		   to avoid negative differences *) 
		let mdd_diff = Mlglu.mdd_eq_minus mgr id'' id1 id2 in 
		let mdd_ispos = Mlglu.mdd_geq mgr id1 id2 in 
		Mlglu.mdd_and mdd_diff mdd_ispos 1 1
	      end
	    | (Mod, _)       ->
		(* the second argument of Mod must be an integral constant *)
		raise SymbolicTypeError

	    | _              -> raise InternalError	  
	  in 
	  let m'  = conj_mdd_list mgr m ml in 
	  let m'' = Mlglu.mdd_smooth_list mgr m' idl in
	  List.iter (Symtemp.release mgr) idl; 
	  MultiMdd (id'', m'')
	end
  in


  (** Given an op, a flag, a variable id, an integer c, 
    a list idl of variable ids to quantify (sublist of [id]), 
    and an mdd list ml, build an mdd for: 

    \exists idl . (id op c  \wedge ml) if flag = false
    \exists idl . (c  op id \wedge ml) if flag = true 

    It also takes care of the case when the integer c is negative. 
   *)

  let build_v_c 
    (op: Ast.binop) 
    (flag: bool) 
    (id: varid_t) 
    (c: int) 
    (idl: varid_t list) 
    (ml: Mlglu.mdd list) : predd = 

    let mdd_false = Mlglu.mdd_zero mgr in 
    let mdd_true  = Mlglu.mdd_one  mgr in 
    match op with 
      Eq | Ne | Gt | Ge | Lt | Le -> 
	begin
	  let m = match (op, flag) with
	      (* Here we could use Serdar's function, e.g., in place of 
		 mdd_gt_c, it would be build_gt_c.  Try later what works best. *)
	      (Eq, _)     -> 
		if c >= 0 then Mlglu.mdd_eq_c  mgr id c
		else mdd_false 
	    | (Ne, _)     -> 
		if c >= 0 then Mlglu.mdd_neq_c mgr id c
		else mdd_true 
	    | (Gt, false) -> 
		if c >= 0 then Mlglu.mdd_gt_c  mgr id c
		else mdd_true 
	    | (Gt, true)  -> 
		if c >= 0 then Mlglu.mdd_lt_c  mgr id c
		else mdd_false 
	    | (Ge, false) -> 
		if c >= 0 then Mlglu.mdd_geq_c mgr id c
		else mdd_true 
	    | (Ge, true)  -> 
		if c >= 0 then Mlglu.mdd_leq_c mgr id c
		else mdd_false 
	    | (Lt, false) -> 
		if c >= 0 then Mlglu.mdd_lt_c  mgr id c
		else mdd_false 
	    | (Lt, true)  -> 
		if c >= 0 then Mlglu.mdd_gt_c  mgr id c
		else mdd_true 
	    | (Le, false) -> 
		if c >= 0 then Mlglu.mdd_leq_c mgr id c
		else mdd_false 
	    | (Le, true)  -> 
		if c >= 0 then Mlglu.mdd_geq_c mgr id c
		else mdd_true 
	    | _           -> raise InternalError
	  in
	  (* conjoin [m] with each mdd in the list [ml] *)
	  let m'  = conj_mdd_list mgr m ml in 
	  (* existentially quantify over temporary variables *)
	  let m'' = Mlglu.mdd_smooth_list mgr m' idl in 
	  List.iter (Symtemp.release mgr) idl; 
	  RelMdd (m'') 
	end
    | And | Or | Implies -> raise SymbolicTypeError
    | Plus | Minus | Mod -> 
	(** Gets a temporary variable *) 
	(* let n_bits = Mlglu.mdd_get_var_bits mgr id in *)
	(* Printf.printf " n_bits=%d, range(id)=%d \n" n_bits
	  (Mlglu.mdd_get_var_range mgr id); 
	   flush stdout; *)
	let id'' = Symtemp.allocate mgr max_bits in
	(* Printf.printf " allocated %s, range=%d \n"
	  (Mlglu.mdd_get_var_name mgr id'')
	   (Mlglu.mdd_get_var_range mgr id''); *)
	(* Printf.printf " n_bits=%d, range(id\'\')=%d \n" n_bits
	  (Mlglu.mdd_get_var_range mgr id''); *)

	let m = match (op, flag) with 
	    (Plus, _)      when c >= 0 ->                         (* id + c *) 
	      Mlglu.mdd_eq_plus_c  mgr id'' id c

	  | (Minus, false) when c <= 0 ->                         (* id - (-c) *)
	      Mlglu.mdd_eq_plus_c  mgr id'' id (-c)

	  | (Minus, true ) when c < 0  ->                         (* (-c) -id *) 
	      (* This is always negative. 
		 As variables cannot have negative values, 
		 we return the false mdd, to encode the fact that
		 id'' can never be equal to the answer. *)  
	      mdd_false  

	  | (Plus, _)      (* when c < 0 *)                       (* id + (-c) *)
	  | (Minus, false) (* when c > 0 *) ->                    (* id - c *)
	      (* The following code is complicated by the need to
		 avoid the function Mlglu.mdd_eq_minus_c_mod.
		 That function cannot handle variables with different
		 ranges, and our temporary variables generally have
		 greater ranges than the original variables. *)
	      let d = abs c in
	      let id' = Symtemp.allocate mgr max_bits in
	      (* mdd_cc is an MDD for id' = d *)
	      let mdd_d = Mlglu.mdd_literal mgr id' [d] in 
	      (* Mlglu.mdd_print mgr mdd_d; *)
	      (* mdd_dif is an MDD for id'' = id - id' *)
	      let mdd_dif = Mlglu.mdd_eq_minus mgr id'' id id' in 
	      (* mdd_nonneg is an MDD for id >= id' *) 
	      let mdd_nonneg = Mlglu.mdd_geq mgr id id' in 
	      (* Computes \exists id' . (mdd_d \and mdd_dif \and mdd_nonneg) *)
	      let m = Mlglu.mdd_and mdd_dif mdd_nonneg 1 1 in 
	      let mtemp = Mlglu.mdd_and_smooth_list mgr mdd_d m [id'] in 
	      Symtemp.release mgr id'; 
	      mtemp

	  | (Minus, true ) (* when c >= 0 *) ->                   (* c - id *) 
	      (* You may not believe it, but computing c - id requires
		 jumping through hoops... *)
	      let id' = Symtemp.allocate mgr max_bits in
	      (* mdd_cc is an MDD for id' = c *)
	      let mdd_c = Mlglu.mdd_literal mgr id' [c] in 
	      (* mdd_dif is an MDD for id'' = id' - id *)
	      let mdd_dif = Mlglu.mdd_eq_minus mgr id'' id' id in 
	      (* mdd_nonneg is an MDD for id' >= id *) 
	      let mdd_nonneg = Mlglu.mdd_geq mgr id' id in 
	      (* Computes \exists id' . (mdd_c \and mdd_dif \and mdd_nonneg) *)
	      let m = Mlglu.mdd_and mdd_dif mdd_nonneg 1 1 in 
	      let mtemp = Mlglu.mdd_and_smooth_list mgr mdd_c m [id'] in 
	      Symtemp.release mgr id'; 
	      mtemp

	  (* We take the mod wrt. the absolute value of the
	     constant *) 
	  | (Mod, _)       -> Mlglu.mdd_mod mgr id'' id (abs c) 

	  | _              -> raise InternalError	
	in 
	(* DEBUG *)
	(* Mlglu.mdd_print mgr m;  *)
	let m'  = conj_mdd_list mgr m ml in 
	let m'' = Mlglu.mdd_smooth_list mgr m' idl in 
	List.iter (Symtemp.release mgr) idl; 
	MultiMdd (id'',m'')
  in


  (* remember at the end to release the temp vars if any!! *)

  (** This function, given an Ast and a symbolic top level, builds a
    pre-MDD for the Ast *)
  let rec build_predd_step (exp: Ast.t) : predd = 

    (** DEBUG message *)
    if debug then begin
      Printf.printf "Building symbolic representation for expression ";
      Ast.print exp;
      Printf.printf "\n";
      flush stdout;
    end; 
    
    match exp with 

      Variable (v, primed, _) -> 
	let id = Symprog.get_id_of_var s v primed in 
	begin
	  match Var.get_type v with
	    Var.Bool -> RelMdd(Mlglu.mdd_literal mgr id [1])
	  | x        -> Varid (id, x)
	end

    | Int (c,_) -> Constant c

    | Tval (true,_)  -> RelMdd(Mlglu.mdd_one mgr) 

    | Tval (false,_) -> RelMdd(Mlglu.mdd_zero mgr) 

    | Unop (Not, a, _) -> 
	begin
	  let b = build_predd_step a in 
	  match b with 
	    Constant c -> raise SymbolicTypeError
	  | RelMdd m -> RelMdd (Mlglu.mdd_not m)
	  | Varid (id, Var.Bool) -> 
	      RelMdd (Mlglu.mdd_literal mgr id [0])
	  | Varid (_,_) -> raise SymbolicTypeError
	  | MultiMdd (id, m) -> raise SymbolicTypeError
	end

    | Unop (Nondet, v, _) -> RelMdd (Mlglu.mdd_one mgr) 
	
    | Binop (op, lt, rt, _) -> 
	begin
	  let ltv = build_predd_step lt in 
	  let rtv = build_predd_step rt in 
	  match (ltv, rtv) with 
	    (* cases for Varid *)

	    (Varid (id, Var.Bool), Varid (id', Var.Bool)) -> build_v_v_bool op id id'
	  | (Varid (id, t), Varid (id', t')) -> 
	      Var.assert_int t; 
	      Var.assert_int t'; 
	      build_v_v_int op false id id' [] []

	  | (Varid (id, t), Constant c) -> Var.assert_int t; build_v_c op false id c [] []
	  | (Constant c, Varid (id, t)) -> Var.assert_int t; build_v_c op true  id c [] []

	  | (Varid (id, t), RelMdd m) -> Var.assert_bool t; build_v_r op false id m 
	  | (RelMdd m, Varid (id, t)) -> Var.assert_bool t; build_v_r op true  id m 

	  | (Varid (id, t), MultiMdd (id', m)) -> 
	      Var.assert_int t; 
	      build_v_v_int op false id id' [id'] [m]

	  | (MultiMdd (id', m), Varid (id, t)) -> 
	      Var.assert_int t; 
	      build_v_v_int op true  id id' [id'] [m]
	      
	  (* cases for MultiMdd without Varid *)

	  | (MultiMdd (id, m), MultiMdd (id', m')) -> build_v_v_int op false id id' [id; id'] [m; m']
	      
	  | (MultiMdd (id, m), Constant c) -> build_v_c op false id c [id] [m]
	  | (Constant c, MultiMdd (id, m)) -> build_v_c op true  id c [id] [m]
	      
	  | (MultiMdd (id, m), RelMdd m') -> raise SymbolicTypeError
	  | (RelMdd m', MultiMdd (id, m)) -> raise SymbolicTypeError

	  (* cases for RelMdd without Varid, MultiMdd *)

	  | (RelMdd m, RelMdd m')  -> build_r_r op m m' 

	  | (RelMdd m, Constant c) -> raise SymbolicTypeError
	  | (Constant c, RelMdd m) -> raise SymbolicTypeError

          (* and finally... *)

	  | (Constant c, Constant c') -> build_c_c op c c' 
	end
  in
  build_predd_step e
;;


(** Builds the mdd corresponding to the expression [e].
  Raises InternalError if [e] is not boolean. *)
let mk_bool (s: Symprog.t) (e: Ast.t) : Mlglu.mdd =
  match build_predd s e with
    RelMdd m -> m
  | _ ->
      Printf.printf " mk_bool only accepts a boolean expression ";
      Ast.print e;
      raise InternalError
;;


(** Builds the symbolic representation of the conjunction of a list
  of expressions (think invariants). *)
let conjoin_exp_list (sp: Symprog.t) (l: Ast.t list) : Mlglu.mdd = 
  let mgr = Symprog.get_mgr sp in
  let mdd = ref (Mlglu.mdd_one mgr) in
  let conjoin_one_exp e =
    let sym_e = mk_bool sp e in 
    mdd := Mlglu.mdd_and !mdd sym_e 1 1
  in
  List.iter conjoin_one_exp l;
  !mdd
;;


(** Builds the symbolic representation of the disjunction of a list
    of guarded commands.
    Variables that are in [wvars], but are not mentioned primed 
    in a command, retain their value.
    This is used to build the transition relation 
    of output and local rules. *)
let disjoin_gc_list (sp: Symprog.t) (l: (Ast.t * Ast.t) list) 
    (wvars: (string, Var.t) Hsetmap.t)   
  : Mlglu.mdd = 
  let mgr = Symprog.get_mgr sp in
  let mdd = ref (Mlglu.mdd_zero mgr) in

  let disjoin_one_gc ((g, c) : (Ast.t * Ast.t)) : unit =
    let symg = mk_bool sp g 
    and symc = mk_bool sp c in
    (* conjoin guard and command *)
    let g_and_c = Mlglu.mdd_and symg symc 1 1 in
    (* make sure unmentioned vars retain their value *)
    let wvars_in_c = Ast.get_primed_vars_hsetmap c in
    let unch_vars = Hsetmap.unsafe_difference wvars wvars_in_c in
    (* translate to ids *)
    let unch_var_ids = Symprog.get_id_of_varset sp unch_vars
    in
    let g_and_c_and_unch = Symutil.and_unchngd sp g_and_c unch_var_ids in
    (* disjoin this command with the previous ones *)
    mdd := Mlglu.mdd_or g_and_c_and_unch !mdd 1 1
  in
  if l = []
    (* empty gc lists give rise to "true" *) 
  then Mlglu.mdd_one mgr 
  else begin
    List.iter disjoin_one_gc l;
    !mdd
  end
;;


(** Builds the symbolic representation of the disjunction of a list
    of deterministic guarded commands.

    Differently from the normal guarded commands,
    the list of deterministic guarded commands has a strict priority,
    i.e. a command is tried only if the previous ones in the list
    do not apply.
    
    Variables that are in [wvars], but are not mentioned primed 
    in a command, retain their value. *)
let disjoin_det_gc_list (sp: Symprog.t) (l: (Ast.t * (Ast.t list)) list) 
  (wvars: (string, Var.t) Hsetmap.t)
  : Mlglu.mdd = 

  let mgr = Symprog.get_mgr sp in
  let mdd = ref (Mlglu.mdd_zero mgr) in
  (* the priority condition *)
  let not_previous_cmds = ref (Mlglu.mdd_one mgr) in

  let disjoin_one_gc ((g, c) : (Ast.t * (Ast.t list))) : unit =

    let symg = mk_bool sp g
    and symc = ref (Mlglu.mdd_one mgr) 
    and wvars_in_c = Hsetmap.mk () in

    (* takes care of one assignement *)
    let conjoin_one_assignement ass = (* :-) *)
      let sym_ass = mk_bool sp ass in
      symc := Mlglu.mdd_and !symc sym_ass 1 1;
      (* now collect the set of primed variables *)
      let wvars_in_ass = Ast.get_primed_vars_hsetmap ass in
      Hsetmap.add_to_first wvars_in_c wvars_in_ass
    in
    (* conjoin in [symc] all assignments comprising the command [c] *)
    List.iter conjoin_one_assignement c;
    (* enforce priority of earlier commands *)
    let new_symg = Mlglu.mdd_and symg !not_previous_cmds 1 1 in
    (* update the priority condition *)
    let not_symg = Mlglu.mdd_not symg in
    not_previous_cmds := Mlglu.mdd_and !not_previous_cmds not_symg 1 1;
    (* conjoin guard and command *)
    let g_and_c  = Mlglu.mdd_and new_symg !symc 1 1 in
    (* make sure that variables in [wvars] that are not mentioned in [c]
       retain their value *)
    let unch_vars  = Hsetmap.unsafe_difference wvars wvars_in_c in
    (* translate to ids *)
    let unch_var_ids = Symprog.get_id_of_varset sp unch_vars in
    let g_and_c_and_unch = Symutil.and_unchngd sp g_and_c unch_var_ids in
    (* disjoin this command with the previous ones *)
    mdd := Mlglu.mdd_or g_and_c_and_unch !mdd 1 1
  in
  if l = []
    (* empty gc lists give rise to "true" *) 
  then Mlglu.mdd_one mgr
  else begin
    List.iter disjoin_one_gc l;

    (* Use the portion below if you decide that the local part should not
       be blocking, so that when no guard applies, nothing changes.
       let otherwise_nothing = Symutil.and_unchngd sp !not_previous_cmds
       (Symprog.get_id_of_varset sp wvars) in 
       Mlglu.mdd_or !mdd otherwise_nothing 1 1 
     *)
    !mdd
  end
;;


(** Builds the symbolic representations of the statesets, and adds
    them to the symbolic module *)
let mk_sset (sp: Symprog.t) (sm: Symmod.t) (name: string) (e: Ast.t) : unit = 
  let mdd = mk_bool sp e in 
  Hsetmap.add (Symmod.get_ssets sm) name mdd

(** Builds the symbolic representation of the local transition rule
 [l], and adds it to the symbolic module [sm]. 

  Defaults:
  - if a guarded command does not mention x', x retains its value.
  - if the rule body is empty (as in "local a: { }"), it is equivalent
  to "local a: {true ==> true }"
*)
let mk_lrule (sp: Symprog.t) (sm: Symmod.t) (r: Rule.lrule_t) : unit =
  let act = Rule.get_lname r in
  (* This is the name of the module where the action originated *)
  let mod_name = Symmod.get_name sm in 
  let gc_list = Rule.get_lgc_list r in
  let wvars   = Rule.get_lwvars r in
  let mdd     = disjoin_gc_list sp gc_list wvars in
  (* propagate set of written vars *)
  let sym_wvars = Symprog.get_id_of_varset sp wvars in
  (* add the resulting rule to the symbolic module [sm] *)
  let symrule = Symmod.mk_lrule act mod_name sym_wvars mdd in
  Symmod.add_rule sm symrule
;;


(** Builds the symbolic representation of the input transition rule
    [r], and adds it to the symbolic module [sm].

    Defaults:
    - global part: if a guarded command does not mention x' (global),
    x is free to change arbitrarily;
    - local part: if a guarded command does not mention x' (local),
    x retains its value. 

    If the global part is empty, it is taken to mean "true".
*)
let mk_irule (sp: Symprog.t) (sm: Symmod.t) (r: Rule.irule_t) : unit =
  let act = Rule.get_iname r in
  let mgr = Symprog.get_mgr sp in

  (* global part of the rule *)
  let g_gc_list = Rule.get_global_igc_list r in
  (* apply defaults: unmentioned (global) vars are free to change *)
  let empty_varset = Hsetmap.mk () in
  let g_mdd = disjoin_gc_list sp g_gc_list empty_varset in 

  (* local part of the rule *)
  let l_gc_list : (Ast.t * (Ast.t list)) list
    = Rule.get_local_igc_list r in
  (* apply defaults: unmentioned (local) vars retain their value *)
  let wvars     = Rule.get_iwlvars r in
  let l_mdd     = disjoin_det_gc_list sp l_gc_list wvars in 
  (* Conjoins with the global guarded command the formula 
     \exists  wvars' . l_mdd 
     This moves the blocking from the local part, to the global part, 
     where it belongs. *) 
  let sym_wvars  = Symprog.get_id_of_varset sp wvars in
  let sym_wvars' = Symprog.prime_vars sp sym_wvars in
  let local_nonblock = Mlglu.mdd_smooth mgr l_mdd sym_wvars' in 
  let g_mdd = Mlglu.mdd_and g_mdd local_nonblock 1 1 in 
  (* add the resulting rule to the symbolic module [sm] *)
  (** In the following line, 
      should it be l_mdd (as it is) or local_nonblock?? - Marco *)
  let symrule = Symmod.mk_irule act sym_wvars g_mdd l_mdd in
  Symmod.add_rule sm symrule;
;;


(** Builds the symbolic representation of the output transition rule 
  [r], and adds it to the symbolic module [sm]. 

  Defaults:
  if a guarded command does not mention x', x retains its value.
*)
let mk_orule (sp: Symprog.t) (sm: Symmod.t) (r: Rule.orule_t) : unit =
  let act = Rule.get_oname r in
  (* This is the name of the module where the action originated *)
  let mod_name = Symmod.get_name sm in 
  let gc_list = Rule.get_ogc_list r in
  (* get the set of all variables that are mentioned primed in any line of [r] *)
  let wvars   = Rule.get_owvars r in
  let mdd     = disjoin_gc_list sp gc_list wvars in
  (* propagate set of written vars *)
  let sym_wvars = Symprog.get_id_of_varset sp wvars in
  (* add the resulting rule to the symbolic module [sm] *)
  let symrule = Symmod.mk_orule act mod_name sym_wvars mdd in
  Symmod.add_rule sm symrule
;;

(** If an output rule does not match any input rule of the module,
    then we add an input rule of the same name, with transition
    relation "false", to explicitly reject the input.  This has been
    added so that regular expressions as input action names work
    properly in composition. *)
let mk_reject_rule (sp: Symprog.t) (sm: Symmod.t) (r: Rule.orule_t) : unit = 
  (* Check if there is a matching input rule *)
  let a_name = Rule.get_oact r in 
  if (Symmod.best_rule_match sm a_name) = None then
    (* There is no input rule.  Makes one, with transition relation "false". *)
    Symutil.mk_false_irule sp sm a_name
;;


(** Adds the environment move that can change the value of
  history-free variables. *)
let mk_env_rule (sp: Symprog.t) (sm: Symmod.t) : unit =
  let mgr = Symprog.get_mgr sp in
  
  (* Global part: history vars keep their value. 
     For economy, we don't say anything about local variables
     (it is enough to set wvars to the empty set *)
  let hvars = Symmod.get_hvars sm in
  let lvars = Symmod.get_lvars sm in 
  let unch_vars = VarSet.diff hvars lvars in 
  let g_mdd = Symutil.and_unchngd sp (Mlglu.mdd_one mgr) unch_vars in
  (* pretend that no local var is mentioned, so that all retain their value *)
  let wvars = VarSet.empty in
  let l_mdd   = Mlglu.mdd_one mgr in
  (* add the resulting rule to the symbolic module [sm] *)
  let symrule = Symmod.mk_irule Symprog.env_act wvars g_mdd l_mdd in
  Symmod.add_rule sm symrule
;;


(** Builds a symbolic module out of module [m]. *)
let mk_mod (m: Mod.t) (sp: Symprog.t) : Symmod.t =
  let mgr = Symprog.get_mgr sp in
  (* Makes a new module. *) 
  let sym_mod = Symmod.mk mgr (Mod.get_name m) in

  (* First, makes a list of clock variables, to assign the bits in 
     the manager in an interleaved way. *)
  let get_clocks vname v clock_l = 
    if (Var.is_clock v) 
    then v :: clock_l 
    else clock_l 
  in
  (* Clocks are only local, so we fold on the local vars only *) 
  let clock_l = Hsetmap.fold get_clocks (Mod.get_lvars m) [] in 
  (* Create bdd ids for the clocks *) 
  Symprog.add_var_list sp clock_l;

  (* Sets the set of clock variables of the symbolic module, 
     with their bounds *) 
  let add_clock v = 
    Symmod.add_cvar sym_mod (Symprog.get_id_of_var sp v false) (Var.nvals v)
  in List.iter add_clock clock_l; 

  (* Makes BDD IDs for all the variables of the module, and 
     adds them to the variable list of the module *)
  let add_one_var v =
    Symprog.add_var_list sp [v];
    let id = Symprog.get_id_of_var sp v false in
    Symmod.add_var sym_mod id 
  in
  Mod.iter_vars m add_one_var;

  (* Now fixes the lvars, gvars and hvars of the module *)
  let add_var_to_lgh n v = 
    let id = Symprog.get_id_of_var sp v false in
    if Hsetmap.mem (Mod.get_lvars m) n
    then Symmod.add_lvar sym_mod id
    else Symmod.add_gvar sym_mod id; 
    if (Hsetmap.mem (Mod.get_hvars m) n) then Symmod.add_hvar sym_mod id
  in Hsetmap.iter add_var_to_lgh (Mod.get_vars m); 

  (* invariants *)
  let iinv = Mod.get_iinv m in
  let sym_iinv = conjoin_exp_list sp iinv in
  Symmod.set_iinv sym_mod sym_iinv;

  let oinv = Mod.get_oinv m in
  let sym_oinv = conjoin_exp_list sp oinv in
  Symmod.set_oinv sym_mod sym_oinv;

  (* initial condition *)
  let init = Mod.get_init m in 
  let sym_init = conjoin_exp_list sp init in 
  Symmod.set_init sym_mod sym_init; 

  (* statesets *)
  Mod.iter_ssets  m (mk_sset  sp sym_mod); 

  (* and transitions *)
  Mod.iter_lrules m (mk_lrule sp sym_mod);
  Mod.iter_irules m (mk_irule sp sym_mod);
  Mod.iter_orules m (mk_orule sp sym_mod);

  (* For all the output rules that are not matched by an input
     rule, adds an input rule with "false" as transition relation, so
     that a module explicitly rejects the inputs it cannot accept.
     This has been added, due to the introduction of regular
     expressions as input rules. *)
  Mod.iter_orules m (mk_reject_rule sp sym_mod); 

  (* Makes the environment rule *)
  mk_env_rule sp sym_mod;

  sym_mod
;;

(** Returns the symbolic representation of the set of states
  satisfying the given expression. *)
let parse_stateset (sp: Symprog.t) (exp_string: string) : stateset_t =
  let lexbuf = Lexing.from_string exp_string in
  let ast = Ticparse.stateset Ticlex.token lexbuf in
  Typecheck.type_check_bool ast;
  mk_bool sp ast
