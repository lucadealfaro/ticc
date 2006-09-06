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


(** Should not happen; report it. *) 
exception WrongRuleType;;

(** Time is not supported yet. *) 
exception NoTimedSupport;; 

exception Bad_typ;;
exception No_input;;

(** This exception is generated if composing modules that are not
    composable. *) 
exception Modules_not_composable 

(** The modules being composed share an action that is local.  This
    exception should not occur. *) 
exception Shared_Local_Rule

(** this exception indicates that the modules being composed are
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


(** has_disjoint_lactions
    This function checks if two modules [m1] and [m2]
  have the same signature.
    The result is true if the intersection between the two
    sets is empty, otherwise it is false.
 *)
let have_same_signature (m1:Symmod.t) (m2:Symmod.t) : bool = 
  let gvars1 = Symmod.get_gvars m1
  and gvars2 = Symmod.get_gvars m2 
  and hvars1 = Symmod.get_hvars m1
  and hvars2 = Symmod.get_hvars m2 in
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
      
(** Checks whether module [m1] refines module [m2]. *)
let refines (sp: Symprog.t) (m1: Symmod.t) (m2: Symmod.t) : bool =
  if not (have_same_signature m1 m2) then false
  else
    let mgr = Symprog.get_mgr sp in
    let allvars = VarSet.union (Symmod.get_vars m1) (Symmod.get_vars
      m2) in
    let iinv2  = Symmod.get_iinv m2 in
    let iinv2' = Symutil.prime_mdd sp m2 iinv2 in
    let iinv1  = Symmod.get_iinv m1 in
    let iinv1' = Symutil.prime_mdd sp m1 iinv1 in
    let oinv2  = Symmod.get_oinv m2 in
    let oinv2' = Symutil.prime_mdd sp m2 oinv2 in
    let oinv1  = Symmod.get_oinv m1 in
    let oinv1' = Symutil.prime_mdd sp m1 oinv1 in
    
    let sim_pre (set: stateset_t) : stateset_t =
      Symutil.assert_no_primed sp set;
      let set' = Symutil.prime_mdd_vars sp set allvars in

      (* build the input simulation constraint *)
      let inp_term = ref (Mlglu.mdd_one mgr) in
      let add_one_irule (r2:rule_t) : unit =
	let (tran2_g, tran2_l) = Symmod.get_rule_ig_il_mdds r2 in
	let tran2 = Mlglu.mdd_and tran2_g tran2_l 1 1 in
	let tran2' = Mlglu.mdd_and tran2 iinv2' 1 1 in
	(* look for a corresponding input transition in m1 *)
	let tran1' = 
	  match Symmod.best_rule_match m1 (Symmod.get_rule_act r2) with
	    None -> Mlglu.mdd_zero mgr
	  | Some r1 -> 
	      let (tran1_g, tran1_l) = Symmod.get_rule_ig_il_mdds r1 in
	      let tran1 = Mlglu.mdd_and tran1_g tran1_l 1 1 in
	      Mlglu.mdd_and tran1 iinv1' 1 1
	in
	let rhs = Mlglu.mdd_and tran1' set' 1 1 in
	let implication = Mlglu.mdd_or tran2' rhs 0 1 in
	let term = Mlglu.mdd_smooth mgr implication (Symmod.get_lvars m1) in
	let term' = Mlglu.mdd_consensus mgr term (Symmod.get_vars m2) in
	inp_term := Mlglu.mdd_and !inp_term term' 1 1
      in
      Symmod.iter_irules m2 add_one_irule; 

      (* build the output simulation constraint *)
      let out_term = ref (Mlglu.mdd_one mgr) in
      let add_one_orule r1 : unit =
	let tran1 = Symmod.get_orule_mdd r1 in
	let tran1' = Mlglu.mdd_and tran1 oinv1' 1 1 in
	(* look for a corresponding output transition in m2 *)
	let tran2' = 
	  try
	    let r2_list = Symmod.get_orule m2 (Symmod.get_rule_act r1) in
	    let r2_mdd_list = List.map Symmod.get_orule_mdd r2_list in
	    let conjoin_mdds a b = Mlglu.mdd_and a b 1 1 in
	    let tran2 = List.fold_left conjoin_mdds 
	      (Mlglu.mdd_one mgr) r2_mdd_list in
	    Mlglu.mdd_and tran2 oinv2' 1 1 
	  with Not_found -> Mlglu.mdd_zero mgr
	in
	let rhs = Mlglu.mdd_and tran2' set' 1 1 in
	let implication = Mlglu.mdd_or tran1' rhs 0 1 in
	let term = Mlglu.mdd_smooth mgr implication (Symmod.get_lvars m2) in
	let term' = Mlglu.mdd_consensus mgr term (Symmod.get_vars m1) in
	out_term := Mlglu.mdd_and !out_term term' 1 1
      in
      Symmod.iter_orules m1 add_one_orule; 

      let both_terms = Mlglu.mdd_and !inp_term !out_term 1 1 in
      Mlglu.mdd_and set both_terms 1 1
    in
    let refinement = ref (Mlglu.mdd_one mgr) in
    refinement := sim_pre !refinement;
    false
    
