(** This module contains operations on ticc modules, implemented 
    symbolically.  The operations are for pre/post, support for I/O
    games, and support for CTL. *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;

type stateset_t = Mlglu.mdd;;
module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 


(** Time is not supported yet. *) 
exception NoTimedSupport;; 


(** This function conjoins [set] with the Input and Output invariants 
    of a module [sm]. *)
let conjoin_w_invs (sm: Symmod.t) (set: Mlglu.mdd) : Mlglu.mdd = 
  let iinv = Symmod.get_iinv sm in 
  let oinv = Symmod.get_oinv sm in 
  Mlglu.mdd_and (Mlglu.mdd_and set iinv 1 1) oinv 1 1 


(** This function builds the transition relation of a rule, NOT 
    including the invariant (for flexibility). 

  Used by Zeno module, among others.

    Warning: there is a more efficient way of doing this for
    pre/post. *)
let get_transition_rel_noinv sp (sm: Symmod.t) (r: rule_t) : Mlglu.mdd = 
  let (glob_tr, loc_tr) = get_rule_tran_as_pair r in 
  let w = get_rule_wvars r in
    match get_rule_type r with 
	Local | Output -> begin 
	  let allvars = Symmod.get_vars sm in 
	  let notw = VarSet.diff allvars w in 
	  (* let uncha = Symbuild.unchngd sp notw in 
	     Mlglu.mdd_and loc_tr uncha 1 1 *)
	  Symutil.and_unchngd sp loc_tr notw
	end
    | Input -> begin 
	(* The transition relation is tr = glob_tr /\ loc_tr /\ uncha, 
	   where uncha is an assertion saying that the local
	   variables that are not mentioned do not change their
	   value. *) 
	let alllocal = Symmod.get_lvars sm in
	let notw = VarSet.diff alllocal w in
	let unchanged_local = Symutil.unchngd sp notw in
	let tr_tmp = Mlglu.mdd_and glob_tr loc_tr 1 1 in 
	let tr = Mlglu.mdd_and tr_tmp unchanged_local 1 1 in
	tr
      end

(* **************************************************************** *)
(*                                                                  *)
(* Pre / Post Operators                                             *)
(*                                                                  *)
(* **************************************************************** *)

(* **************************************************************** *)
(* Existential flavor *)

(** (May) Pre for a rule.  
    Given a set of states [set] and a rule [r], 
    this function computes the set of states that
    may reach [set] in one application of [r].
    In other words, the set of states that have at least one successor
    that satisfies rule [r] and belongs to [set].

    [sm] is the module for which this is done, and [sp] is the
    symbolic program top. 
    [set] must be a predicate over _unprimed_ vars. 

    Note: the result is NOT conjoined with the set of I or O invariants. 
 *)
let pre_rule (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
    (r: Symmod.rule_t) : stateset_t = 
    
    let mgr = Symprog.get_mgr sp in
    (* w is the set of variables that are changed, for output or local
       rules, and for input rules, it is the set of local variables that
       are changed.  See symmod.ml for more information *) 
    let w  = Symmod.get_rule_wvars r in 
    let w' = Symprog.prime_vars sp w in 
    
    match (Symmod.get_rule_tran r) with 
      Symmod.Loc tau | Symmod.Out tau -> 
	  (* conjoin [set] with the output invariant,
             as the Output player cannot escape it *)
	  let valid_set = Mlglu.mdd_and set (Symmod.get_oinv sm) 1 1 in
	  (* set' = valid_set [w' / w] *)
	  let set' = Symutil.prime_mdd_vars sp valid_set w in 
	  (* the result is \exists W' . (\tau /\ set') *) 
	  Mlglu.mdd_and_smooth mgr tau set' w' 
    | Symmod.Inp (taug, taul) -> 
	  (* conjoin [set] with the input invariant,
             as the Input player cannot escape it *)
	  let valid_set = Mlglu.mdd_and set (Symmod.get_iinv sm) 1 1 in
	  (* gets global variables of the module *) 
	  let gv = Symmod.get_gvars sm in 
	  let gv' = Symprog.prime_vars sp gv in 
	  (* The result is: 
	     \exists gv' . (\tau_I_g \und \exists w' . 
	     (\tau^I_l \und set [gv', w' / gv, w])). 
	     Computes this in stages *) 
	  let v = VarSet.union w gv in 
	  let set' = Symutil.prime_mdd_vars sp valid_set v in 
	  let partial = Mlglu.mdd_and_smooth mgr taul set' w' in 
	  Mlglu.mdd_and_smooth mgr taug partial gv' 

(** Internal Pre for all rules. 
    Given a set of states [set], returns the set of states that have a
    transition to [set]. 
    If [do_input] is true, does all input rules. 
    If [do_output] is true, does all output rules. 
    Note: the result is NOT conjoined with any invariant. 
 *)
let internal_pre (do_input: bool) (do_output: bool) (sp: Symprog.t) (sm: Symmod.t) 
	(set: stateset_t) : stateset_t = 
    let mgr = Symprog.get_mgr sp in
    let result = ref (Mlglu.mdd_zero mgr) in 
    let do_one_rule r: unit = 
      let new_pre = pre_rule sp sm set r in 
      result := Mlglu.mdd_or !result new_pre 1 1 
    in 
    if do_input then Symmod.iter_irules sm do_one_rule;
    if do_output then begin
      Symmod.iter_orules sm do_one_rule; 
      Symmod.iter_lrules sm do_one_rule
    end;
    !result

(** Pre for Input, Output, and all rules. 
    To be used only for CTL: they are conjoined with the I and O invarnants.
    Use [internal_pre] otherwise. 
 *)
let e_input_pre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_pre true false sp sm set)

let e_output_pre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_pre false true sp sm set)

let e_pre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_pre true true sp sm set)

	    
(** Given a set of states [passage_set] and a set of states
    [target_set], this function computes the set of states 
    that satisfies the formula [passage_set] Until [target_set] in 0,
    1, or more application of rule [r]. 
    [sm] is the module for which this is done, and [sp] is the
    symbolic program top. 
    [passage_set] and [target_set] must be predicates over _unprimed_ vars. 

    The function returns a pair, consisting of the new set, and of a flag, 
    telling us whether the set has been enlarged. 

    Note that no invariant is conjoined with the set that is returned. 
 *)
let rule_until (sp: Symprog.t) (sm: Symmod.t) 
    (passage_set: stateset_t) (target_set: stateset_t) 
    (r: Symmod.rule_t) : stateset_t * bool = 
  
  let mgr = Symprog.get_mgr sp in
  (* w is the set of variables that are changed, for output or local
     rules, and for input rules, it is the set of local variables that
     are changed.  See symmod.ml for more information *) 
  let w  = Symmod.get_rule_wvars r in 
  let w' = Symprog.prime_vars sp w in 
  
  match (Symmod.get_rule_tran r) with 
    Symmod.Loc tau | Symmod.Out tau -> 
      (* awkward variable declaration *)
      let tot_set = ref target_set in 
      let frontier = ref target_set in 
      let enlarged = ref false in 
      while not (Mlglu.mdd_is_zero !frontier) do
	let frontier_and_oinv = Mlglu.mdd_and !frontier (Symmod.get_oinv sm) 1 1 in 
	(* set' = frontier_and_oinv [w' / w] *)
	let set' = Symutil.prime_mdd_vars sp frontier_and_oinv w in 
	(* pre_front = \exists W' . (\tau /\ set') *) 
	let pre_front = Mlglu.mdd_and_smooth mgr tau set' w' in
	let pre_front' = 
	  if Mlglu.mdd_is_one passage_set 
	  then pre_front
	  else Mlglu.mdd_and pre_front passage_set 1 1 
	in
	frontier := Mlglu.mdd_and pre_front' !tot_set 1 0; 
	if not (Mlglu.mdd_is_zero !frontier) then enlarged := true; 
	tot_set  := Mlglu.mdd_or  !tot_set !frontier 1 1
      done;
      (!tot_set, !enlarged)
  | Symmod.Inp (taug, taul) -> 
      (* awkward variable declaration *)
      let tot_set = ref target_set in 
      let frontier = ref target_set in 
      let enlarged = ref false in 
      (* gets global variables of the module *) 
      let gv = Symmod.get_gvars sm in 
      let gv' = Symprog.prime_vars sp gv in 
      let v = VarSet.union w gv in 
      while not (Mlglu.mdd_is_zero !frontier) do
	(* The result is: 
	   \exists gv' . (\tau_I_g \und \exists w' . 
	   (\tau^I_l \und frontier [gv', w' / gv, w])). 
	   Computes this in stages *) 
	let frontier_and_iinv = Mlglu.mdd_and !frontier (Symmod.get_iinv sm) 1 1 in 
	let set' = Symutil.prime_mdd_vars sp frontier_and_iinv v in 
	let partial = Mlglu.mdd_and_smooth mgr taul set' w' in 
	let pre_front = Mlglu.mdd_and_smooth mgr taug partial gv' in 
	let pre_front' = 
	  if Mlglu.mdd_is_one passage_set 
	  then pre_front
	  else Mlglu.mdd_and pre_front passage_set 1 1 
	in
	frontier := Mlglu.mdd_and pre_front' !tot_set 1 0; 
	if not (Mlglu.mdd_is_zero !frontier) then enlarged := true; 
	tot_set  := Mlglu.mdd_or  !tot_set !frontier 1 1
      done;
      (!tot_set, !enlarged)
	

(** Given a set of states [passage_set] and a set of states
    [target_set], this function computes the set of states 
    that satisfies the formula [passage_set] Until [target_set] in 0,
    1, or more application of rules. 
    If [do_input] is true, it considers all input rules. 
    If [do_output] is true, it considers all output rules. 

    Algorithm: uses rule_until, cycling over all local and output rules
    until the set cannot be enlarged any further. 

    Note: the answer is not conjoined with any invariant. 
*)
let internal_until  (do_input: bool) (do_output: bool) 
	(sp: Symprog.t) (sm: Symmod.t) 
	(passage_set: stateset_t) (target_set: stateset_t) : stateset_t =
    (* start with "true" *)
    let result = ref (target_set) in 
    let enlarged = ref true in 
    
    let do_one_rule r : unit = 
	let (result', e') = rule_until sp sm passage_set !result r in 
	result := result';
	enlarged := !enlarged || e' 
    in 

    (* Cycles over all rules, until the set cannot be enlarged any further *)
    while !enlarged do
	enlarged := false;
	if do_input then Symmod.iter_irules sm do_one_rule;
	if do_output then begin
	    Symmod.iter_orules sm do_one_rule; 
	    Symmod.iter_lrules sm do_one_rule
	end
    done; 
    !result


let lo_pre_star sp sm = 
    let mgr = Symprog.get_mgr sp in
    internal_until false true sp sm (Mlglu.mdd_one mgr) 

let i_pre_star sp sm = 
    let mgr = Symprog.get_mgr sp in
    internal_until true false sp sm (Mlglu.mdd_one mgr) 

(** CTL operator for exists until *)
let e_until (sp: Symprog.t) (sm: Symmod.t) 
    (passage_set: stateset_t) (target_set: stateset_t) : stateset_t =
  let set = internal_until true true sp sm passage_set target_set in 
  conjoin_w_invs sm set 

(** Algorithm: \nu X . (target_set \union (passage_set \inters epre(X)))
 *)
let e_waitfor (sp: Symprog.t) (sm: Symmod.t) 
	(passage_set: stateset_t) (target_set: stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    let result = ref (Mlglu.mdd_zero mgr) in 
    let frontier = ref (Mlglu.mdd_one mgr) in 
    result := Mlglu.mdd_or target_set passage_set 1 1;
    frontier := !result; 
    while not (Mlglu.mdd_is_zero !frontier) do 
	let passg_and_pre = Mlglu.mdd_and passage_set (internal_pre true true sp sm !result) 1 1 in 
	let targ_union_above = Mlglu.mdd_or passg_and_pre target_set 1 1 in 
	frontier := Mlglu.mdd_and !result targ_union_above 1 0;
	result := targ_union_above
    done; 
    conjoin_w_invs sm !result


(* **************************************************************** *)
(* Universal flavor *)

(** Forall-pre computation. 
    Given a set [set] of states, we compute the set of states all
    whose output/local successors are in [set], 
    for the given module [sm] in the symbolic top [sp]. 
    It returns a symbolic representation of the result. 

    Algorithm: 

    Let \phi be the set, and R be the set of output or local rules of [sm]. 

    lo_apre (X) = [ \bigwedge_{r \in R} \neg \Pre_r (\neg X) ]
 *)

let internal_apre  (do_input: bool) (do_output: bool) 
	(sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =

    let mgr = Symprog.get_mgr sp in
    let result = ref (Mlglu.mdd_one mgr) in
    let not_set = Mlglu.mdd_not set in
    let do_one_rule r : unit =
	let tmp  = pre_rule sp sm not_set r in
	result  := Mlglu.mdd_and !result tmp 1 0;
    in
    if do_input then  
	Symmod.iter_irules sm do_one_rule;
    if do_output then begin
	Symmod.iter_orules sm do_one_rule;
	Symmod.iter_lrules sm do_one_rule
    end; 
    !result

(** These operators are to be used only for CTL, since they conjoin the result with the invariant *)

let lo_apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_apre false true sp sm set)

let i_apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_apre true false sp sm set)

let apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
  conjoin_w_invs sm (internal_apre true true sp sm set)


(** Computes \forall (passage_set Until target_set)
    via: 
    \mu X . ( target_set \union (passage_set \inters a_pre (X) ))
 *)
let a_until (sp: Symprog.t) (sm: Symmod.t) 
  (passage_set: stateset_t) (target_set: stateset_t) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let result = ref (Mlglu.mdd_zero mgr) in 
  let frontier = ref (Mlglu.mdd_zero mgr) in 
  result := target_set; 
  frontier := !result; 
  while not (Mlglu.mdd_is_zero !frontier) do 
    let apre_frontier = internal_apre true true sp sm !result in 
    let frontier' = Mlglu.mdd_and apre_frontier passage_set 1 1 in 
    frontier := Mlglu.mdd_and frontier' !result 1 0; 
    result   := Mlglu.mdd_or  !frontier  !result 1 1
  done; 
  !result
    

(* **************************************************************** *)
(* Post and reachability *)

(** Computes the post of set [set] wrt a rule [r] of module [sm].  
    The flag conj_inv specifies whether we have to conjoin the
    invariant to the result (normally, yes). *)
let post_rule (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t)
    (r: Symmod.rule_t) (conj_inv: bool) : stateset_t =

  (* TODO: update for timed modules *)
  (*Printf.printf "applying rule %s\n" (Symmod.get_rule_act r);*)

  let mgr = Symprog.get_mgr sp in 
    
  let w  = Symmod.get_rule_wvars r in 
  let allvars = Symmod.get_vars sm in 
  let allvars'= Symprog.prime_vars sp allvars in
  let local = Symmod.get_lvars sm in  
  
  match (Symmod.get_rule_tran r) with 
    
    Symmod.Loc tau | Symmod.Out tau -> 
      (* the result is 
	 (\exists w . (\tau /\ set)[notw'/notw] )[allvars/allvars'] ) 
         /\ oinv
       *)
      let notw = VarSet.diff allvars w in 
      let set_tau = Mlglu.mdd_and set tau 1 1 in 
      let set_tau_notw' =  Symutil.prime_mdd_vars sp set_tau notw in
      let result  = Mlglu.mdd_smooth mgr set_tau_notw' w in
      let unprimed_result = Symutil.unprime_mdd_vars sp result allvars' in
      if conj_inv then begin
	let oinv = Symmod.get_oinv sm in
	Mlglu.mdd_and unprimed_result oinv 1 1
      end
      else unprimed_result
	
  | Symmod.Inp (taug, taul) -> 
      
      (* the result is
	 ( (\exists allvars . 
	 (taug /\ taul /\ set)[(V^l \ w)'/(V^l \ w)] )[allvars/allvars'] ) /\ iinv
	 where w is the set of variables that can be rewritten by taul
       *)
      let local_notw = VarSet.diff local w in
      let taul_set      = Mlglu.mdd_and set taul 1 1 in 
      let taul_set_taug = Mlglu.mdd_and taul_set taug 1 1 in 
      let result_prime_notw = 
	Symutil.prime_mdd_vars sp taul_set_taug local_notw in
      let result = Mlglu.mdd_smooth mgr result_prime_notw allvars in
      let unprimed_result = Symutil.unprime_mdd_vars sp result allvars' in
      if conj_inv then begin
	let iinv = Symmod.get_iinv sm in
	Mlglu.mdd_and unprimed_result iinv 1 1
      end
      else unprimed_result
	
(** Takes the local/output post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local and output rules of a module [sm].
    [set] is a predicate over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached.
    Again, conj_inv states whether we have to conjoin the invariants 
    (normally, yes). 
 *)
let lo_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) (conj_inv: bool) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let result = ref (Mlglu.mdd_zero mgr) in
  let do_one_rule (r: Symmod.rule_t) : unit =
    let dest = post_rule sp sm set r conj_inv in 
    result := Mlglu.mdd_or !result dest 1 1
  in
  Symmod.iter_lrules sm do_one_rule;
  Symmod.iter_orules sm do_one_rule;
  !result


(** Takes the local post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local rules of a module [sm].
    [set] is a predicate over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached.
    Again, conj_inv states whether we have to conjoin the invariants 
    (normally, yes). 
 *)
let l_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) (conj_inv: bool) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let result = ref (Mlglu.mdd_zero mgr) in
  let do_one_rule (r: Symmod.rule_t) : unit =
    let dest = post_rule sp sm set r conj_inv in 
    result := Mlglu.mdd_or !result dest 1 1
  in
  Symmod.iter_lrules sm do_one_rule;
  !result


(** Takes the input post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local and output rules of a module [sm].
    [set] is a predicat over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached.
    Again, conj_inv states whether we have to conjoin the invariants 
    (normally, yes). 
 *)
let i_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) (conj_inv: bool) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let result = ref (Mlglu.mdd_zero mgr) in

  let do_one_rule (r: Symmod.rule_t) : unit =
    let dest = post_rule sp sm set r conj_inv in 
    result := Mlglu.mdd_or !result dest 1 1
  in
  Symmod.iter_irules sm do_one_rule;
  !result

(** Takes the timed post of a set of states [set]. 
    This function computes the set of states that can be reached
  from the set [set] by letting time advance by 1,
  provided doing so does not violate invariants. *)
let time_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) :
  stateset_t =
  let mgr = Symprog.get_mgr sp in
  let c_vars      = Symmod.get_cvars sm in
  let c_vars'     = Symprog.prime_vars sp c_vars in
  let c_var_list  = Vset.to_list c_vars in
  let c_var_list' = Vset.to_list c_vars' in

  (*  (\exists Clocks . set /\ delta1)[Clocks / Clocks'] *)
  let delta1 = Symmod.get_delta1 sm in
  let temp = Mlglu.mdd_and set delta1 1 1 in
  let temp = Mlglu.mdd_smooth mgr temp c_vars in
  let temp = Mlglu.mdd_substitute_two_lists mgr temp c_var_list' c_var_list in
  (* conjoin with invariants *)
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let invs = Mlglu.mdd_and iinv oinv 1 1 in
  Mlglu.mdd_and temp invs 1 1


(** This does both i_post and lo_post, conjoining invariants. *)
let loi_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
  let lo_succ  = lo_post sp sm set true in 
  let i_succ = i_post sp sm set true in
  let loi_succ = Mlglu.mdd_or lo_succ i_succ 1 1 in
  if is_timed sm then
    let time_succ = time_post sp sm set in
    Mlglu.mdd_or loi_succ time_succ 1 1
  else
    loi_succ

(** Computes the post of a set [set], not conjoining invariants *)
let raw_post (do_input: bool) (do_output: bool)
    (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
  let mgr = Symprog.get_mgr sp in
  let result = ref (Mlglu.mdd_zero mgr) in 
  if do_input then result := i_post sp sm set false; 
  if do_output then 
    result := Mlglu.mdd_or !result (lo_post sp sm set false) 1 1; 
  !result


(* **************************************************************** *)

(** This function returns the set of states that are reachable from a given 
    initial set. *)
let reach (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t = 
    let mgr = Symprog.get_mgr sp in
    (* awkward variable declaration *)
    let tot_set = ref (Mlglu.mdd_zero mgr) in 
    let frontier = ref (Mlglu.mdd_zero mgr) in 
    tot_set := set; 
    frontier := set;
    while not (Mlglu.mdd_is_zero !frontier) do
	let set' = loi_post sp sm !frontier in 
	frontier := Mlglu.mdd_and set' !tot_set 1 0; 
	tot_set  := Mlglu.mdd_or !tot_set !frontier 1 1 
    done;
    !tot_set

(** This function returns the set of reachable states of a module *)
let reachable  (sp: Symprog.t) (sm: Symmod.t) : Mlglu.mdd = 
    match sm.reachset with 
	Some r -> r 
      | None -> begin
	    let r = reach sp sm sm.init in 
	    sm.reachset <- Some r; 
	    r
	end



(* **************************************************************** *)
(* Winning functions for games *)

(** Winning set of a safety game for Input.  
  This is the least efficient implementation; 
    see win_i_safe below for a faster one. 
    Given a set [set] of states and a module [sm], this function
    computes the set of states from which the Input player has
    a strategy to stay in the set [set].
    It returns a symbolic representation of the result.
    
    Algorithm:
    
    \nu X.\; [set \wedge \forall\pre^O(X)]

    REMARKS: [set] should be included in both invariants.
             To win a safety game, it's better not to move. 
 *)
let win_i_safe_alt_1 (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =

    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre_set = internal_apre false true sp sm !result in
	let contains = Mlglu.mdd_or !result apre_set 0 1 in
	if Mlglu.mdd_is_one contains then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre_set 1 1;
    done;
    !result


(** Winning set of a safety game for Input.
    Given a set [set] of states and a module [sm], this function
    computes the set of states from which the Input player has
    a strategy to stay in the set [set].
    It returns a symbolic representation of the result.
    
    Algorithm:
    
    1. Complement set
    2. Enlarge the complement, using lo_pre_star
    3. Complement the result. 

    REMARKS: [set] should be included in both invariants.
    If [set] is included in both invariants, so is the result of 
    this function, since lo_pre_star never shrinks a set. 
    To win a safety game, it's better not to move. 
 *)
let win_i_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    Mlglu.mdd_not (lo_pre_star sp sm (Mlglu.mdd_not set))


(** Winning set of a safety game for Output.
    There is a more efficient implementation; see below. 
    Given a set [set] of states and a module [sm], this function
    computes the set of states from which the Output/Local player has
    a strategy to stay in the set [set].
    It returns a symbolic representation of the result.
    
    Algorithm
    
    \nu X.\; [set \wedge \forall\pre^I(X)]

    REMARKS: [set] should be included in both invariants.
             To win a safety game, it's better not to move. 
 *)
let win_lo_safe_alt_1 (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =

    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre_set = internal_apre true false sp sm !result in
	let contains = Mlglu.mdd_or !result apre_set 0 1 in
	if Mlglu.mdd_is_one contains then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre_set 1 1;
    done;
    !result

(** This is the faster alternative.  [set] should be included in both
    invariants, and this guarantees that also the return value is.  *)
let win_lo_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
  Mlglu.mdd_not (i_pre_star sp sm (Mlglu.mdd_not set))

