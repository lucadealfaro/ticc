(** This module computes the composition of modules *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;
open Ops;;

module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 


(** Should not happen; report it. *) 
exception WrongRuleType;;

(** Time is not supported yet. *) 
exception NoTimedSupport;; 

exception Bad_typ;;
exception No_input;;

(** This exception is generated if composing modules that are not
    composable. *) 
exception Modules_not_composable 

(** The modules being composed share an action that is local.  
    This exception should not occur. *) 
exception Shared_Local_Rule

(** This exception indicates that the modules being composed are
    incompatible. *)
exception Incompatible_Modules

(** Diagnostics *) 
exception Internal_error_1
exception Internal_error_2
exception Internal_error_3


(** **************** Composition of modules  ******************* *)


(** has_disjoint_lvars: Check for disjoint sets of local variables.
    This function checks if two modules [m1] and [m2]
    have different sets of local variables.
    The result is true if the intersection between the two
    sets is empty, otherwise it is false.

    Remark: if [m1] and [m2] share local variables, then the shared set
    is given by a printout.
 *)
let has_disjoint_lvars (sp:Symprog.t) (m1:Symmod.t) (m2:Symmod.t) : bool =
    let lvars_1 = Symmod.get_lvars m1 in
    let lvars_2 = Symmod.get_lvars m2 in
    let common_lvars = VarSet.inter lvars_1 lvars_2 in
    if not (VarSet.is_empty common_lvars) then 
      begin
	Printf.printf "\nModules %s and %s share the following set of local variables \n" (Symmod.get_name m1) (Symmod.get_name m2);
	Symutil.print_varset sp common_lvars
      end;
    VarSet.is_empty common_lvars
;;

(** has_disjoint_lactions: Check for disjoint sets of local actions.
    This function checks if two modules [m1] and [m2]
    have different sets of local actions.
    The result is true if the intersection between the two
    sets is empty, otherwise it is false.


 *)

let has_disjoint_lactions (m1:Symmod.t) (m2:Symmod.t) : bool = 
    let result = ref true in
    let next_local_rule (m : Symmod.t) (r : rule_t): unit = 
        let action = get_rule_act r in 
        if !result && (has_action m action) then 
	begin
	Printf.printf "Modules %s and %s share the same local 
        action %s\n" (Symmod.get_name m1) (Symmod.get_name m2) 
        (Symmod.get_rule_act r);
	flush stdout;
	result := false
	end
    in 
    Symmod.iter_lrules m1 (next_local_rule m2); 
    !result;;


(** is_composable.
    This functions checks if two modules [m1] and [m2]
    satisfy the composability condition.
    
    The composability condition is given by
    
    "Marco's condition": 

    \forall a \in \Actions_2 . 
    W_2(a) \cap \vstate{1} \neq \emptyset \implies a \in \Actions_1 
    \forall a \in \Actions_1 .
    W_1(a) \cap \vstate{2} \neq \emptyset \implies a \in \Actions_2. 

    and    V_1^L \cap V_2^L {\equal} {\empty}

    and     local actions of both modules are not shared.

    The function return true if [m1] and [m2] satisfy the 
    composability condition. Otherwise, it returns false.

    Notice that the function is only iterated on output rules.
    This is because output rules are the only rules
    that can rewrite a global variable.

    REMARK: Each composability problem is reported by a printout. 
 *)
let is_composable (sp:Symprog.t) (m1:Symmod.t) (m2:Symmod.t) : bool =
    let result = ref true in
    (* v_i [sm] returns the variables that have state for module [sm],
       be they local or global. *)
    let v_i sm = VarSet.union (Symmod.get_lvars sm) (Symmod.get_hvars sm) in
    let v_1 = v_i m1 in
    let v_2 = v_i m2 in
    let interferes wvars v = not (VarSet.is_empty (VarSet.inter wvars v)) in
    let check_for_interference sm1 sm2 sm2_v (r: rule_t) =
	if !result then begin
	    (* if result is false, we already know that [m1] and [m2] 
	       are not composable*)
	    let act = Symmod.get_rule_act r in
	    let wvars = Symmod.get_rule_wvars r in
	    if (interferes wvars sm2_v)
		&& not (Symmod.has_input_action_wild sm2 act)
	    then begin  
		Printf.printf "Modules %s and %s have a composability problem.\n" 
		    (Symmod.get_name sm1) (Symmod.get_name sm2); 
		flush stdout;
		Printf.printf "Action %s of Module %s modifies variables that are also part of Varset of module %s.\nThe set of variables that is modified is " act (Symmod.get_name sm1) (Symmod.get_name sm2);
		flush stdout;
		Symutil.print_varset sp (VarSet.inter wvars sm2_v);
		Printf.printf ".\n";
		Printf.printf "However, Module %s does not have action %s\n \n" 
		    (Symmod.get_name sm2) act;
		flush stdout;
		result := false
            end
	end
    in
    Symmod.iter_orules m1 (check_for_interference m1 m2 v_2);
    Symmod.iter_orules m2 (check_for_interference m2 m1 v_1);
    !result && has_disjoint_lvars sp m1 m2 && has_disjoint_lactions m1 m2
;;


(** Product. 
    This function computes the product of two modules [m1] 
    and [m2]. The order is given by [m1] product [m2].
    
    The function takes an argument [result_name] that can
    be used to name the result of the product. 

 *)
let product (sp: Symprog.t) ?(result_name="") (m1: Symmod.t) (m2: Symmod.t)
	: Symmod.t =
    let mgr = Symprog.get_mgr sp in
    (*if the user does not choose a name, the concatenation
      of the names of the two modules is taken*)
    let sm_name =
	if result_name = ""
	then (Symmod.get_name m1) ^ "*" ^ (Symmod.get_name m2)
	else result_name
    in
    let m12 = Symmod.mk mgr sm_name in

    (* First, takes care of the basic things: variables, invariants,
       initial condition, stateses.
       Note that this code follows the product definition (def 15)                  
       given in the FROCOS 05 paper.
     *)
    (* Variables *)
    Symmod.set_vars  m12 (VarSet.union (Symmod.get_vars  m1) (Symmod.get_vars  m2)); 
    Symmod.set_lvars m12 (VarSet.union (Symmod.get_lvars m1) (Symmod.get_lvars m2));
    Symmod.set_gvars m12 (VarSet.union (Symmod.get_gvars m1) (Symmod.get_gvars m2));
    Symmod.set_hvars m12 (VarSet.union (Symmod.get_hvars m1) (Symmod.get_hvars m2));
    Symmod.set_cvars m12 (VarSet.union (Symmod.get_cvars m1) (Symmod.get_cvars m2));
    (* Clock bounds *)
    Symmod.set_clk_bounds m12 
	(Hsetmap.unsafe_union (Symmod.get_bounds m1) (Symmod.get_bounds m2));
    (* Invariants *)
    Symmod.set_oinv m12 (Mlglu.mdd_and (Symmod.get_oinv m1) (Symmod.get_oinv m2) 1 1); 
    let conj_iinv = Mlglu.mdd_and (Symmod.get_iinv m1) (Symmod.get_iinv m2) 1 1 in 
    Symmod.set_iinv m12 conj_iinv; 
    Symmod.set_old_iinv m12 conj_iinv; 
    (* Initial condition *)
    Symmod.set_init m12 (Mlglu.mdd_and (Symmod.get_init m1) (Symmod.get_init m2) 1 1);
    (* Time transition *)
    Symmod.set_delta1 m12 (Mlglu.mdd_and (Symmod.get_delta1 m1) (Symmod.get_delta1 m2) 1 1); 

    (* Statesets *)
    Symmod.set_ssets m12
	(Hsetmap.unsafe_union (Symmod.get_ssets m1) (Symmod.get_ssets m2));
    (* The reachset we leave it alone; it is computed only upon request *)
    (* Ok, now all that remains to be done are the rules *)

    (* This function adds a new rule in the product.
       The function is called when the action that labels
       the rule [r] does not synchronize with any action in the 
       other module [other_sm].
     *)
    let add_nonsynch_action (other_sm: Symmod.t) (r: Symmod.rule_t) : unit = 

	(* the resulting rule is the same the original rule, for Loc & Out *)
	let new_rule =
	    match (Symmod.get_rule_tran r) with
		Loc _	-> r
	      | Out _	-> r
	      | Inp (mdd_ig, mdd_il) ->
		    (* this is the tricky case, because we have two MDDs to
		       update, and they need updating in different
		       ways.  The problem is that the local and global
		       portions need to say that the variables of the
		       _other_ module keep their value.  Also, wvars
		       needs to be fixed. 
		     *)
		    let other_hvars = get_hvars other_sm in
		    (* The global variables of the other module cannot
		       change.  This comes from the FROCOS paper, and
		       the reason is subtle: if an output transition
		       of the environment by the same name changes
		       some variable in other_hvars, then the other
		       module should know about the transition!
		       Hence, we put as condition that other_hvars
		       should not be changed. *)
		    let new_mdd_ig = Symutil.and_unchngd sp mdd_ig other_hvars in
		    (* for the local part, wvars keeps track of which
		       variables can change, so there is no need to
		       change the transition relation. *) 
		    let new_wvars = r.wvars in
		    Symmod.mk_irule (Symmod.get_rule_act r) new_wvars mdd_ig mdd_il
	in
	add_rule m12 new_rule
    in

    (* The function adds a new rule to the product. 
       It is called for a pair of input rules that synchronize. 
       The new name of the action (due to regexp matching) is passed as an
       argument. 
     *)
    let add_input_input_synch_action (r1: Symmod.rule_t) (r2: Symmod.rule_t) 
	    (new_name: string) : unit = 

	(* the resulting rule is the same the original rule, for Loc & Out *)
	(* gets the mdds *)
	let (rho1_ig, rho1_il) = Symmod.get_rule_ig_il_mdds r1 in
	let (rho2_ig, rho2_il) = Symmod.get_rule_ig_il_mdds r2 in
	(* gets the wvars *)
	let wvars1 = Symmod.get_rule_wvars r1 in 
	let wvars2 = Symmod.get_rule_wvars r2 in
	(* Computes the new wvars and transition relations. 
	   Fortunately, the obvious works. *)
	let new_wvars = VarSet.union wvars1 wvars2 in 
	let new_ig = Mlglu.mdd_and rho1_ig rho2_ig 1 1 in 
	let new_il = Mlglu.mdd_and rho1_il rho2_il 1 1 in 
	(* Inserts the new rule *)
	Symmod.add_rule m12 (Symmod.mk_irule new_name new_wvars new_ig new_il)
    in 

    (* The function adds a new output rule to the product. 
       It is called for a pair of input/output rules that synchronize. 
       The new name of the action is taken from the name of the output action. 
     *)
    let add_input_output_synch_action (inp_r: Symmod.rule_t) (out_r: Symmod.rule_t) 
	    : unit = 
	(* First, checks that ir is indeed an input rule, and or is indeed 
	   an output rule. *)
	if (Symmod.get_rule_type inp_r) != Input 
	    || (Symmod.get_rule_type out_r) != Output then raise WrongRuleType;
	(* Ok, now computes the new rule. *)
	let owner_mod = Symmod.get_owner_module out_r in 
	(* Modified variables *)
	let i_wvars = Symmod.get_rule_wvars inp_r in 
	let o_wvars = Symmod.get_rule_wvars out_r in 
	let io_wvars = VarSet.union i_wvars o_wvars in 
	(* Transition relation *)
	let rho_o = Symmod.get_orule_mdd out_r in 
	let (_, rho_il) = Symmod.get_rule_ig_il_mdds inp_r in
	(* Now they must be combined *)
	(* Is io_wvars fine?  Or do we have to add some unchanged variables? 
	   I think it is fine.  In fact, o_wvars specifies which global and local1 
	   variables change, i_wvars specifies which local2 variables can change. *)
	let rho_io = Mlglu.mdd_and rho_o rho_il 1 1 in 
	(* The name is taken from the output rule (due to possible regexp in the input one). *)
	let act_io = Symmod.get_rule_act out_r in 
	(* Builds the rule *)
	let new_r = Symmod.mk_orule act_io owner_mod io_wvars rho_io in 
	(* Now it adds the rule.  Note that a module can have more
	   than one rule with the same name, as a result of 
	   an input-output synchronization in the other direction. *)
	Symmod.add_rule m12 new_r
    in 

    (* Ok, now that these helper functions have been declared, 
       we need to go over the rules of the two modules, and add the resulting rules 
       to the new module m12. *)

    (* Let's start from local rules. *)
    let add_locals1 (r: rule_t) : unit = add_nonsynch_action m2 r in 
    Symmod.iter_lrules m1 add_locals1;
    let add_locals2 (r: rule_t) : unit = add_nonsynch_action m1 r in 
    Symmod.iter_lrules m1 add_locals2;

    (* Then, outputs of 1 with inputs of 2 *)
    let match_output_rule1 (r1: rule_t) : unit = 
	match Symmod.best_rule_match m2 (Symmod.get_rule_act r1) with 
	  Some r2 ->  add_input_output_synch_action r2 r1
	| None -> add_nonsynch_action m2 r1
    in Symmod.iter_orules m1 match_output_rule1; 

    (* ... outputs of 2 with inputs of 1 ... *)
    let match_output_rule2 (r2: rule_t) : unit = 
	match Symmod.best_rule_match m1 (Symmod.get_rule_act r2) with 
	  Some r1 -> add_input_output_synch_action r1 r2
	| None -> add_nonsynch_action m1 r2
    in Symmod.iter_orules m2 match_output_rule2;

    (* For inputs, I keep a hash table of the pairs that have already been done. *)
    let done_rules = Hset.mk () in 
    
    (* Inputs of 1 with inputs of 2 *)
    let match_input_rule1 (r1: rule_t) : unit = 
	let act1 = Symmod.get_rule_act r1 in 
	match Symmod.best_rule_match m2 act1 with 
	  Some r2 -> begin
	      let act2 = Symmod.get_rule_act r2 in 
	      if not (Hset.mem done_rules (act1, act2))
	      then begin
		  Hset.add done_rules (act1, act2); 
		  add_input_input_synch_action r1 r2 act1
	      end
	  end
	| None -> add_nonsynch_action m2 r1
    in Symmod.iter_irules m1 match_input_rule1; 

    (* Inputs of 2 with inputs of 1 *)
    let match_input_rule2 (r2: rule_t) : unit = 
	let act2 = Symmod.get_rule_act r2 in
	match Symmod.best_rule_match m1 act2 with 
	  Some r1 -> begin
	      let act1 = Symmod.get_rule_act r1 in 
	      if not (Hset.mem done_rules (act1, act2))
	      then begin
		  Hset.add done_rules (act1, act2); 
		  add_input_input_synch_action r1 r2 act2
	      end
	  end
	| None -> add_nonsynch_action m1 r2
    in Symmod.iter_irules m2 match_input_rule2; 

    (* All done *)
    m12


(** composition.

    This function compose two modules [m1] and [m2].
    The order is given by [m1] compose [m2]
    
    The function takes an argument [result_name] that can
    be used to name the result of the composition. 
    
    Remark. The composition is done in 4 steps:
    1. Check if [m1] and [m2] are composable.
    2. Take the product of [m1] and [m2]
    3. Compute the set of good states.
    4. Play the game using the set of good states
 *)

let composition (sp:Symprog.t) win_algo ?(result_name="") (m1: Symmod.t) (m2: Symmod.t)
	: Symmod.t =
    let mgr = Symprog.get_mgr sp in

    (* Verify composability *)
    if not(is_composable sp m1 m2) then raise Modules_not_composable; 

    (* calculate product *)
    let m12 = product sp ~result_name:result_name m1 m2 in

    (* Now we must construct the set of good states, following Definition
       21 (local compatibility) of the FROCOS paper. We do this by
       iterating on the output rules of each module, and if the rule 
       interferes with the other module, we match it with an input
       rule.  We then ask that the output rule transition complies
       with the input rule.  We use an auxiliary function, in order to
       avoid having to write two separate iterations. *)
    let build_good_term (mo: Symmod.t) (mi: Symmod.t) (m12: Symmod.t) : Mlglu.mdd = 
	let vAll_12' = Symprog.prime_vars sp (Symmod.get_vars m12) in
	(* This is the function that will be folded over all output rules *)
	let check_output_rule (ro: rule_t) (good_term: Mlglu.mdd) : Mlglu.mdd = 
	    let ro_wvars = Symmod.get_rule_wvars ro in 
	    (* Find the best match for ro in mi *)
	    match Symmod.best_rule_match mi (Symmod.get_rule_act ro) with 
	      (* Not shared: do nothing *)
	      None -> good_term 
	    | Some ri -> begin 
		  (* The transition of ro must be acceptable by ri *)
		  (* Computes the hatted transition relations *)
		  let rho_o = Symmod.get_orule_mdd ro in 
		  let (rho_ig, rho_il) = Symmod.get_rule_ig_il_mdds ri in 
		  let inv_o = Symmod.get_oinv mo in 
		  let inv_i = Symmod.get_iinv mi in 
		  let allv = Symmod.get_vars m12 in 
		  let hat_rho_o = Mlglu.mdd_and rho_o  (Symutil.prime_mdd_vars sp inv_o allv) 1 1 in 
		  let hat_rho_i = Mlglu.mdd_and rho_ig (Symutil.prime_mdd_vars sp inv_i allv) 1 1 in 
		  (* Computed the unchanged variables relation *)
		  let unch_vars = VarSet.diff (Symmod.get_gvars mi) ro_wvars in 
		  let unch_term = Symutil.unchngd sp unch_vars in 
		  (* (hat_rho_o /\ unch_term) ==> hat_rho_i *)
		  let conj = Mlglu.mdd_and hat_rho_o unch_term 1 1 in 
		  let impl = Mlglu.mdd_or conj hat_rho_i 0 1 in 
		  let good_part = Mlglu.mdd_consensus mgr impl vAll_12' in 
		  let result = Mlglu.mdd_and good_part good_term 1 1 in 
		  result
	      end
	in
	Symmod.fold_orules mo check_output_rule (Mlglu.mdd_one mgr)
    in
    (* Strengthens the invariant of m12 to the set of good states *)
    let term1 = build_good_term m1 m2 m12 in 
    let term2 = build_good_term m2 m1 m12 in 
    let good_states = Mlglu.mdd_and term1 term2 1 1 in 
    let inv12 = Symmod.get_iinv m12 in 
    let new_inv12 = win_algo sp m12 (Mlglu.mdd_and good_states inv12 1 1) in 
    Symmod.set_iinv m12 new_inv12; 
    (* Remembers the set of bad states *)
    Symmod.set_bad_states m12 (Mlglu.mdd_not good_states);
    (* Now it must restrict the initial condition; if it becomes empty, then the 
       modules are incompatible *)
    let vars12 = Symmod.get_vars m12 in 
    let lvars12 = Symmod.get_lvars m12 in 
    let new_init = Mlglu.mdd_and (Symmod.get_init m12)
	(Mlglu.mdd_smooth mgr new_inv12 (VarSet.diff vars12 lvars12)) 1 1 in 
    Symmod.set_init m12 new_init; 
    if Mlglu.mdd_is_zero new_init then begin
	Printf.printf "The initial condition of module %s is empty: this is a sign of incompatibility!\n" 
	    (Symmod.get_name m12); 
	flush stdout
	(* I think it's best not to raise an exception.  This way, one can still print the 
	   modules and try to figure out what is wrong. *)
	(* raise Incompatible_Modules *)
    end;
    (* returns the composition *)
    m12
;;


(* End of functions needed for composition                                   *)
(*---------------------------------------------------------------------------*)


(** composition_test1.

    This function takes a list of modules, and composes them together.
    In standard composition, you iteratively add modules onto a larger
    and larger result.  For instance, modules [m1],[m2],[m3], and [m4]
    are normally composed as: (([m1] + [m2]) + [m3]) + [m4]

    This particular test takes a quicksort view of the world,
    and instead composes the four modules as:
    ([m1] + [m2]) + ([m3] + [m4])
    You can get even smarter than this, by using heuristics
    to choose pairs that are likely to have some restructions.

    The function takes an argument [result_name] that can
    be used to name the result of the composition. 

    Responsible party: Bo Adler

 *)
exception Empty_module_list;;
let rec composition_test1 (sp:Symprog.t) win_algo ?(result_name="")
    (modList : Symmod.t list) : Symmod.t list =
  (* The strategy is to compose every 2 modules, and then recursively
     invoke the parent function to compose the resulting list. *)

  (* Helper function to go through list of modules and compose each
     pair of modules, returning back a list of the compositions.
     If there is an odd number of elements, we just don't compose
     the last one. *)
  let rec composition_helper (modList : Symmod.t list) : Symmod.t list =
    match modList with
      []             -> []
    | [m1]           -> [m1]
    | m1::m2::rest   ->
	let c1 = composition sp win_algo ~result_name:"" m1 m2 in
	let cList = composition_helper rest in
	c1::cList
  in
  let pairs = composition_helper modList in
  match pairs with
    []   -> raise Empty_module_list    (* No modules specified? *)
  | [m1] ->
      (* If there is only a single module remaining, then we are
	 done.  We just have to rename the module. *)
      (* TODO TODO TODO *)
      [m1]
  | _    -> composition_test1 sp win_algo ~result_name:result_name pairs

