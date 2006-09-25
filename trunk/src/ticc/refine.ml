(** This module handles refinement checking between symbolic modules *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;
open Ops;;

type stateset_t = Mlglu.mdd;;
type relationset_t = Mlglu.mdd;;
module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 


(** Time is not supported yet. *) 
exception NoTimedSupport;; 

(** Diagnostics *) 
exception Internal_error


(** This function checks if two modules [m1] and [m2]
  have the same signature, which is a prerequisite to refinement.
 *)
let have_same_signature (m1:Symmod.t) (m2:Symmod.t) : bool = 
  let gvars1 = Symmod.get_gvars m1
  and gvars2 = Symmod.get_gvars m2 
  and hvars1 = VarSet.diff (Symmod.get_hvars m1) (Symmod.get_lvars m1)
  and hvars2 = VarSet.diff (Symmod.get_hvars m2) (Symmod.get_lvars m2) in
  VarSet.equal gvars1 gvars2 && 
  VarSet.equal hvars1 hvars2 


(** Takes the local post closure of a set of states [set]. 
    This function computes the set of states that can be reached
  from a set [set] by repeated application of any number of local rules
    of a module [sm].
    [set] is a predicate over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached.
 *)
let epsilon_closure (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let tot_set = ref set 
  and frontier = ref set in 
  while not (Mlglu.mdd_is_zero !frontier) do
    let set' = l_post sp sm !frontier true in 
    frontier := Mlglu.mdd_and set' !tot_set 1 0; 
    tot_set  := Mlglu.mdd_or !tot_set !frontier 1 1 
  done;
  !tot_set


(** Takes the star-closure of the union (disjunction) 
    of all local transition relations. 
    This function computes a predicate over primed and unprimed
    _local_ variables of [sm] representing the possible hidden paths,
    of any length, including zero, in module [sm].
    The fact that global variables keep their value during
    local tranitions is _not_ enforced by this predicate.
    The fact that changes in local variables cannot violate
    the output invariant is also _not_ enforced here.
    
    Notice that the output is not properly a "stateset", 
    since it also mentions primed variables, but alas...
    
    Notice that this predicate could (and probably should) be cached
    in the symbolic module, for repeated refinement computations.
 *)
let epsilon_closure_pred (sp: Symprog.t) (sm: Symmod.t) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let vars  = Symmod.get_lvars sm in
  let vars' = Symprog.prime_vars sp vars in

  (* l_pred is a predicate representing the disjunction of all
     local transitions of [sm] *)
  let l_pred = 
    let temp = ref (Mlglu.mdd_zero mgr) in 
    let do_one_rule (r: Symmod.rule_t) : unit =
      let (tran, _) = Symmod.get_rule_tran_as_pair r in
      let wvars     = Symmod.get_rule_wvars r in
      let unmentioned = VarSet.diff vars wvars in
      (* enforce the condition that local variables that are not
	 mentioned by this rule keep their value *)
      let tran_and_unchngd = Symutil.and_unchngd sp tran unmentioned in
      temp := Mlglu.mdd_or !temp tran_and_unchngd 1 1
    in
    Symmod.iter_lrules sm do_one_rule;
    !temp 
  in

  (* procure a set of temporary variables *)
  let var_list   = Vset.to_list vars in
  let var_list'  = Vset.to_list vars' in
  let e_var_list = Symprog.get_extra_vars sp var_list in
  let e_vars = Vset.from_list e_var_list in

  (* DEBUG intermezzo *)
  (* Printf.printf " Some sets of variables:\n";
     Symutil.print_varlist_rough sp var_list;
     Symutil.print_varlist_rough sp e_var_list; *)

  (* the closure predicate is computed according to the recursion:
     closure_0 = identity
     closure_{i+1} = closure_i or 
                   ( exists Z l_pred[Z/V^L'] and closure_i[Z/V^L] ),

     where Z is a set of temporary variables, one for each variable in V^L.
   *)
  let closure_pred = ref (Symutil.unchngd sp vars) in
  let frontier = ref (Mlglu.mdd_one mgr) in 

  while not (Mlglu.mdd_is_zero !frontier) do
    let renamed_l_pred = Mlglu.mdd_substitute_two_lists
      mgr l_pred var_list' e_var_list in
    let renamed_closure_pred = Mlglu.mdd_substitute_two_lists
      mgr !closure_pred var_list e_var_list in
    let conj = Mlglu.mdd_and renamed_l_pred renamed_closure_pred 1 1 in
    let new_term = Mlglu.mdd_smooth mgr conj e_vars in
    frontier := Mlglu.mdd_and new_term !closure_pred 1 0; 
    closure_pred := Mlglu.mdd_or !closure_pred new_term 1 1 
  done;
  (* Mlglu.mdd_print mgr !closure_pred; *)
  !closure_pred
;;

      
(** Computes the most general refinement relation between
    [m1] and [m2]. *)
let refinement (sp: Symprog.t) (m1: Symmod.t) (m2: Symmod.t) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  if not (have_same_signature m1 m2) then
    Mlglu.mdd_zero mgr
  else
    (* sets of variables *)
    let vars1 = Symmod.get_vars m1 in
    let vars2 = Symmod.get_vars m2 in
    let vars1' = Symprog.prime_vars sp vars1 in
    let vars2' = Symprog.prime_vars sp vars2 in
    let lvars1 = Symmod.get_lvars m1 in
    let lvars2 = Symmod.get_lvars m2 in
    let lvars2_list = Vset.to_list lvars2 in
    let lvars1' = Symprog.prime_vars sp lvars1 in
    let lvars2' = Symprog.prime_vars sp lvars2 in
    let lvars2_list' = Vset.to_list lvars2' in
    let allvars = VarSet.union vars1 vars2 in

    (* invariants *)
    let iinv2  = Symmod.get_iinv m2 in
    let iinv2' = Symutil.prime_mdd sp m2 iinv2 in
    let iinv1  = Symmod.get_iinv m1 in
    let iinv1' = Symutil.prime_mdd sp m1 iinv1 in
    let oinv2  = Symmod.get_oinv m2 in
    let oinv2' = Symutil.prime_mdd sp m2 oinv2 in
    let oinv1  = Symmod.get_oinv m1 in
    let oinv1' = Symutil.prime_mdd sp m1 oinv1 in

    let closure2 = epsilon_closure_pred sp m2 in
    let e_vars2_list = Symprog.get_extra_vars sp lvars2_list in
    let e_vars2 = Vset.from_list e_vars2_list in
    let renamed_closure2 = Mlglu.mdd_substitute_two_lists
      mgr closure2 lvars2_list' e_vars2_list in

    (* DEBUG intermezo *)
    (* Printf.printf " Some other sets of variables:\n";
       Symutil.print_varlist_rough sp lvars2_list;
       Symutil.print_varlist_rough sp lvars2_list';
       Symutil.print_varlist_rough sp e_vars2_list;
       raise Not_found; *)

    (* the operator whose fixpoint we will be computing *)
    let sim_pre (set: stateset_t) : stateset_t =
      Symutil.assert_no_primed sp set;
      let set' = Symutil.prime_mdd_vars sp set allvars in

      (* build the _input_ simulation constraint,
	 according to line 1 of SimPre in Section 5.3 of the FROCOS paper *)
      let inp_term = ref (Mlglu.mdd_one mgr) in
      let add_one_irule (r2:rule_t) : unit =
	let (tran2_g, tran2_l) = Symmod.get_rule_ig_il_mdds r2 in
	let tran2 = Mlglu.mdd_and tran2_g tran2_l 1 1 in
	let tran2' = Mlglu.mdd_and tran2 iinv2' 1 1 in
	(* look for a corresponding input transition in m1 *)
	(* OPTIMIZATION: the result of the following block should be cached for
	   future iterations of the outer fixpoint loop *) 
	let tran1' = 
	  match Symmod.best_rule_match m1 (Symmod.get_rule_act r2) with
	    None -> Mlglu.mdd_zero mgr
	  | Some r1 -> 
	      let (tran1_g, tran1_l) = Symmod.get_rule_ig_il_mdds r1 in
	      let tran1 = Mlglu.mdd_and tran1_g tran1_l 1 1 in
	      (* conjoin with input invariant 1 *)
	      Mlglu.mdd_and tran1 iinv1' 1 1
	in
	let rhs = Mlglu.mdd_and tran1' set' 1 1 in
	let implication = Mlglu.mdd_or tran2' rhs 0 1 in
	(* apply the quantifiers *)
	let term = Mlglu.mdd_smooth mgr implication lvars1' in
	let term' = Mlglu.mdd_consensus mgr term vars2' in
	inp_term := Mlglu.mdd_and !inp_term term' 1 1
      in
      Symmod.iter_irules m2 add_one_irule; 

      (* build the _output_ simulation constraint,
	 according to line 2 of SimPre in Section 5.3 of the FROCOS
	 paper, enriched with local hidden transitions *)
      let out_term = ref (Mlglu.mdd_one mgr) in
      let closure_and_dest = Mlglu.mdd_and renamed_closure2 set' 1 1 in 
      
      (* consider an output transition r1 of m1 *)
      let add_one_orule r1 : unit =
	let tran1       = Symmod.get_orule_mdd r1 in
	let wvars1      = Symmod.get_rule_wvars r1 in
	let unmentioned = VarSet.diff vars1 wvars1 in	
	let tran1' = Mlglu.mdd_and tran1 oinv1' 1 1 in
	(* enforce the condition that variables that are not
	   mentioned by this rule keep their value *)
	let tran1'' = Symutil.and_unchngd sp tran1' unmentioned in
	(* look for a corresponding output transition in m2, *)
	(* where m2 can also take some local transitions first *)
	(* OPTIMIZATION: the result of the following block should be cached for
	   future iterations of the outer fixpoint loop *) 
	let tran2' = 
	  try
	    (* fetch the list of output transitions with the same
	       label as r1 *)
	    let r2_list = Symmod.get_orule m2 (Symmod.get_rule_act r1)
	    in
	    let tran2 = ref (Mlglu.mdd_one mgr) in
	    let do_one_rule r2 : unit =
	      let wvars2      = Symmod.get_rule_wvars r2 in
	      let unmentioned = VarSet.diff vars2 wvars2 in	
	      let mdd = Symmod.get_orule_mdd r2 in
	      (* enforce the condition that variables that are not
		 mentioned by this rule keep their value *)
	      let mdd' = Symutil.and_unchngd sp mdd unmentioned in
	      tran2 := Mlglu.mdd_and !tran2 mdd' 1 1;
	    in
	    List.iter do_one_rule r2_list;
	    (* conjoin with output invariant 2 *)
	    Mlglu.mdd_and !tran2 oinv2' 1 1 
	  with Not_found -> Mlglu.mdd_zero mgr
	in
	let renamed_tran2 = Mlglu.mdd_substitute_two_lists 
	  mgr tran2' lvars2_list e_vars2_list in
	let rhs = Mlglu.mdd_and renamed_tran2 closure_and_dest 1 1 in
	let implication = Mlglu.mdd_or tran1'' rhs 0 1 in
	(* apply the quantifiers *)
	let term = Mlglu.mdd_smooth mgr implication lvars2' in
	let term' = Mlglu.mdd_smooth mgr term e_vars2 in
	let term'' = Mlglu.mdd_consensus mgr term' vars1' in
	out_term := Mlglu.mdd_and !out_term term'' 1 1
      in
      Symmod.iter_orules m1 add_one_orule; 
      
      (* build the _local_ constraint *)
      let loc_term = ref (Mlglu.mdd_one mgr) in
      (* consider a local transition r1 of m1 *)
      let add_one_lrule r1 : unit =
	let (tran1, _) = Symmod.get_rule_tran_as_pair r1 in
	let wvars1     = Symmod.get_rule_wvars r1 in
	let unmentioned = VarSet.diff vars1 wvars1 in	
	let tran1' = Mlglu.mdd_and tran1 oinv1' 1 1 in
	(* enforce the condition that local variables that are not
	   mentioned by this rule keep their value *)
	let tran1'' = Symutil.and_unchngd sp tran1' unmentioned in
	(* look for a corresponding local path in m2, *)
	let tran2 = Mlglu.mdd_and closure2 oinv2' 1 1 in
	let rhs = Mlglu.mdd_and tran2 set' 1 1 in
	let implication = Mlglu.mdd_or tran1'' rhs 0 1 in
	(* apply the quantifiers *)
	let term = Mlglu.mdd_smooth mgr implication lvars2' in
	let term' = Mlglu.mdd_consensus mgr term vars1' in
	loc_term := Mlglu.mdd_and !loc_term term' 1 1
      in
      Symmod.iter_lrules m1 add_one_lrule;
  
      (* put the three terms together *)
      let first_two = Mlglu.mdd_and !inp_term !out_term 1 1 in
      let all_terms = Mlglu.mdd_and first_two !loc_term 1 1 in
      (* let all_terms = first_two in *)
      Mlglu.mdd_and set all_terms 1 1
    in
    let refinement = ref (Mlglu.mdd_one mgr)
    and cut = ref (Mlglu.mdd_one mgr) in

    (* compute fixpoint of sim_pre *)
    while not (Mlglu.mdd_is_zero !cut) do
      let new_refinement = sim_pre !refinement in
      cut := Mlglu.mdd_and !refinement new_refinement 1 0;
      refinement := new_refinement;
    done;
    !refinement
;;
  
(* DEBUG intermezzo *)
    (* Mlglu.mdd_print mgr !refinement;
       flush stdout; *)

(** [refines sp m1 m2]
    Checks whether module [m1] refines (i.e. can replace) module [m2]. *)
let refines (sp: Symprog.t) (m1: Symmod.t) (m2: Symmod.t) : bool =
  let mgr = Symprog.get_mgr sp in
  let refin = refinement sp m1 m2 in
  (* check whether each initial state of m2 is simulated by an
     initial state of m1 *)
  (* formula: 
     forall V_2^G forall V_2^L exists V_1^L ( I_2 implies I_1 ) *)
  let rhs = Mlglu.mdd_and (Symmod.get_init m1) refin 1 1 in
  let implication = Mlglu.mdd_or (Symmod.get_init m2) rhs 0 1 in
  let temp = Mlglu.mdd_smooth mgr implication (Symmod.get_lvars m1) in
  let temp' = Mlglu.mdd_consensus mgr temp (Symmod.get_vars m2) in
  Mlglu.mdd_is_one temp'
;;    
    
