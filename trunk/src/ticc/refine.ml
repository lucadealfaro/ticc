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

(** this exception indicates that the modules being composed are
    incompatible. *)
exception Incompatible_Modules

(** Diagnostics *) 
exception Internal_error



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
  variables of [sm] representing the possible hidden paths,
  of any length, in module [sm].

  Notice that the output is not properly a "stateset", but alas...
 *)
let epsilon_closure_pred (sp: Symprog.t) (sm: Symmod.t) : stateset_t =
  let mgr = Symprog.get_mgr sp in

  (* l_pred is a predicate representing the disjunction of all
     local transitions of [sm] *)
  let l_pred = 
    let temp = ref (Mlglu.mdd_zero mgr) in 
    let do_one_rule (r: Symmod.rule_t) : unit =
      let (tran, _) = Symmod.get_rule_tran_as_pair r in
      temp := Mlglu.mdd_or !temp tran 1 1
    in
    Symmod.iter_lrules sm do_one_rule;
    !temp 
  in

  let closure_pred = ref l_pred
  and frontier = ref (Mlglu.mdd_one mgr) in 

  (* add an extra temporary variable for each variable of [sm] *)
  (* I know, it's amazing how long this is! *)
  let var_ids  = Symmod.get_vars sm in
  let var_ids' = Symprog.prime_vars sp var_ids in
  let add_one_var (id:int) l = 
    let (var, p) = Symprog.get_var_p sp id in
    var :: l
  in
  let var_list = VarSet.fold add_one_var var_ids [] in
  let build_lists (name_l, nvals_l, strides_l) v = 
      ("extra_" ^ (Var.get_name v)) :: name_l, 
      (Var.nvals v) :: nvals_l, 
      1 :: strides_l 
  in 
  (* We reverse the list so that variables are actually added in the
     order we specify *)
  let (name_l, nval_l, strd_l) = List.fold_left build_lists ([], [], []) (List.rev var_list) in
  let first_id = Mlglu.mdd_create_variables mgr nval_l name_l strd_l in 
  
  let extra_ids = 
    let max_id = first_id + List.length name_l - 1 in
    let temp = ref VarSet.empty in
    for i = first_id to max_id do
      temp := VarSet.add i !temp;
    done;
    !temp
  in
  (* DEBUG *)
  Printf.printf " Some sets of variables:\n";
  Symutil.print_varset_rough sp var_ids;
  Symutil.print_varset_rough sp var_ids';
  Symutil.print_varset_rough sp extra_ids;

  (* the closure predicate is computed according to the recursion:
     closure_0 = false
     closure_{i+1} = closure_i or 
                   ( exists Z l_pred[Z/V^all'] and closure_i[Z/V^all] )
     where Z is a set of temporary variables, one for each variable in V^all.
   *)
  while not (Mlglu.mdd_is_zero !frontier) do
    let renamed_l_pred = Mlglu.mdd_substitute_two_lists 
      mgr l_pred (Vset.to_list var_ids') (Vset.to_list extra_ids) in
    let renamed_closure_pred = Mlglu.mdd_substitute_two_lists 
      mgr !closure_pred (Vset.to_list var_ids) (Vset.to_list extra_ids) in
    let conj = Mlglu.mdd_and renamed_l_pred renamed_closure_pred 1 1 in
    let new_term = Mlglu.mdd_smooth mgr conj extra_ids in
    frontier := Mlglu.mdd_and new_term !closure_pred 1 0; 
    closure_pred := Mlglu.mdd_or !closure_pred new_term 1 1 
  done;
  !closure_pred

      
(** Checks whether module [m1] refines (i.e. can replace) module [m2]. *)
let refines (sp: Symprog.t) (m1: Symmod.t) (m2: Symmod.t) : bool =
  if not (have_same_signature m1 m2) then false
  else
    let mgr = Symprog.get_mgr sp in
    let allvars = VarSet.union (Symmod.get_vars m1) 
      (Symmod.get_vars m2) in
    let iinv2  = Symmod.get_iinv m2 in
    let iinv2' = Symutil.prime_mdd sp m2 iinv2 in
    let iinv1  = Symmod.get_iinv m1 in
    let iinv1' = Symutil.prime_mdd sp m1 iinv1 in
    let oinv2  = Symmod.get_oinv m2 in
    let oinv2' = Symutil.prime_mdd sp m2 oinv2 in
    let oinv1  = Symmod.get_oinv m1 in
    let oinv1' = Symutil.prime_mdd sp m1 oinv1 in
    let closure2 = epsilon_closure_pred sp m2 in
    
    let sim_pre (set: stateset_t) : stateset_t =
      Symutil.assert_no_primed sp set;
      let set' = Symutil.prime_mdd_vars sp set allvars in

      (* build the input simulation constraint,
	 according to line 1 of SimPre in Section 5.3 of the FROCOS paper *)
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
	let term = Mlglu.mdd_smooth mgr implication 
	  (Symprog.prime_vars sp (Symmod.get_lvars m1)) in
	let term' = Mlglu.mdd_consensus mgr term 
	  (Symprog.prime_vars sp (Symmod.get_vars m2)) in
	inp_term := Mlglu.mdd_and !inp_term term' 1 1
      in
      Symmod.iter_irules m2 add_one_irule; 

      (* build the output simulation constraint,
	 according to line 2 of SimPre in Section 5.3 of the FROCOS paper *)
      let out_term = ref (Mlglu.mdd_one mgr) in

      (* consider an output transition r1 of m1 *)
      let add_one_orule r1 : unit =
	let tran1 = Symmod.get_orule_mdd r1 in
	let tran1' = Mlglu.mdd_and tran1 oinv1' 1 1 in
	(* look for a corresponding output transition in m2, *)
	(* where m2 can also take some local transitions first *)
	let tran2' = 
	  try
	    (* fetch the list of output transitions with the same label *)
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
	let term = Mlglu.mdd_smooth mgr implication 
	  (Symprog.prime_vars sp (Symmod.get_lvars m2)) in
	let term' = Mlglu.mdd_consensus mgr term 
	  (Symprog.prime_vars sp (Symmod.get_vars m1)) in
	out_term := Mlglu.mdd_and !out_term term' 1 1
      in
      Symmod.iter_orules m1 add_one_orule; 

      let both_terms = Mlglu.mdd_and !inp_term !out_term 1 1 in
      Mlglu.mdd_and set both_terms 1 1
    in
    let refinement = ref (Mlglu.mdd_one mgr)
    and cut = ref (Mlglu.mdd_one mgr) in

    (* compute fixpoint of sim_pre *)
    while not (Mlglu.mdd_is_zero !cut) do
      let new_refinement = sim_pre !refinement in
      cut := Mlglu.mdd_and !refinement new_refinement 1 0;
      refinement := new_refinement;
    done;
    (* DEBUG *)
    (* Mlglu.mdd_print mgr !refinement; *)
    (* now check whether each initial state of m2 is simulated by an
       initial state of m1 *)
    (* formula: 
       forall V_2^G forall V_2^L exists V_1^L ( I_2 implies I_1 ) *)
    let rhs = Mlglu.mdd_and (Symmod.get_init m1) !refinement 1 1 in
    let implication = Mlglu.mdd_or (Symmod.get_init m2) rhs 0 1 in
    let temp = Mlglu.mdd_smooth mgr implication (Symmod.get_lvars m1) in
    let temp' = Mlglu.mdd_consensus mgr temp (Symmod.get_vars m2) in
    (* Mlglu.mdd_print mgr temp'; *)
    Mlglu.mdd_is_one temp'
    
