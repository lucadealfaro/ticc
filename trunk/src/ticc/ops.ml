(** This module contains operations on ticc modules, implemented 
    symbolically. *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;

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


(* **************************************************************** *)
(*                                                                  *)
(* Pre / Post Operators                                             *)
(*                                                                  *)
(* **************************************************************** *)

(** (May) Pre for a rule.  
    Given a set of states [set] and a rule [r], 
    this function computes the set of states that
    may reach [set] in one application of [r].
    In other words, the set of states that have at least one successor
    that satisfies rule [r] and belongs to [set].

    [sm] is the module for which this is done, and [sp] is the
    symbolic program top. 
    [set] must be a predicate over _unprimed_ vars. 
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
	      
	    
(** (May) Pre* for a rule.  
    Given a set of states [set] and a rule [r], 
    this function computes the set of states that
    may reach [set] in zero, one, or more application of [r].

    [sm] is the module for which this is done, and [sp] is the
    symbolic program top. 
    [set] must be a predicate over _unprimed_ vars. 

    The function returns a pair, consisting of the new set, and of a flag, 
    telling us whether the set has been enlarged. 
 *)
let pre_rule_star (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
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
	  let tot_set = ref (Mlglu.mdd_zero mgr) in 
	  let frontier = ref (Mlglu.mdd_zero mgr) in 
	  let enlarged = ref false in 
	  tot_set := Mlglu.mdd_and set (Symmod.get_oinv sm) 1 1;
	  frontier := !tot_set;
	  while not (Mlglu.mdd_is_zero !frontier) do
	      (* set' = frontier [w' / w] *)
	      let set' = Symutil.prime_mdd_vars sp !frontier w in 
	      (* pre_front = \exists W' . (\tau /\ set') *) 
	      let pre_front = Mlglu.mdd_and_smooth mgr tau set' w' in
	      frontier := Mlglu.mdd_and pre_front !tot_set 1 0; 
	      if not (Mlglu.mdd_is_zero !frontier) then enlarged := true; 
	      tot_set  := Mlglu.mdd_or  !tot_set !frontier 1 1
	  done;
	  (!tot_set, !enlarged)
      | Symmod.Inp (taug, taul) -> 
	  (* awkward variable declaration *)
	  let tot_set = ref (Mlglu.mdd_zero mgr) in 
	  let frontier = ref (Mlglu.mdd_zero mgr) in 
	  let enlarged = ref false in 
	  tot_set := Mlglu.mdd_and set (Symmod.get_iinv sm) 1 1;
	  frontier := !tot_set;
	  (* gets global variables of the module *) 
	  let gv = Symmod.get_gvars sm in 
	  let gv' = Symprog.prime_vars sp gv in 
	  let v = VarSet.union w gv in 
	  while not (Mlglu.mdd_is_zero !frontier) do
	      (* The result is: 
		 \exists gv' . (\tau_I_g \und \exists w' . 
		 (\tau^I_l \und frontier [gv', w' / gv, w])). 
		 Computes this in stages *) 
	      let set' = Symutil.prime_mdd_vars sp !frontier v in 
	      let partial = Mlglu.mdd_and_smooth mgr taul set' w' in 
	      let pre_front = Mlglu.mdd_and_smooth mgr taug partial gv' in 
	      frontier := Mlglu.mdd_and pre_front !tot_set 1 0; 
	      if not (Mlglu.mdd_is_zero !frontier) then enlarged := true; 
	      tot_set  := Mlglu.mdd_or  !tot_set !frontier 1 1
	  done;
	  (!tot_set, !enlarged)
	    
(** Iterated pre on output and local rules.  Given a set [set],
    computes the set of states that can reach [set] in 0, 1, or more
    transitions. 
    If [do_input] is true, it considers input transitions; otherwise, 
    it considers output transitions. 

    Algorithm: uses pre_rule_star, cycling over all local and output rules
    until the set cannot be enlarged any further. *)
let pre_star  (do_input: bool) (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =
    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref (set) in 
    let enlarged = ref true in 
    
    let do_one_rule r : unit = 
	let (result', e') = pre_rule_star sp sm !result r in 
	result := result';
	enlarged := !enlarged || e' 
    in 

    (* Cycles over all rules, until the set cannot be enlarged any further *)
    while !enlarged do
	enlarged := false;
	if do_input then
	    Symmod.iter_irules sm do_one_rule
	else begin 
	    Symmod.iter_orules sm do_one_rule; 
	    Symmod.iter_lrules sm do_one_rule
	end
    done; 
    !result

let lo_pre_star = pre_star false
let i_pre_star  = pre_star true 

(** Forall-pre-output-local.
    Given a set [set] of states, we compute the set of states all
    whose output/local successors are in [set], 
    for the given module [sm] in the symbolic top [sp]. 
    It returns a symbolic representation of the result. 

    Algorithm: 

    Let \phi be the set, and R be the set of output or local rules of [sm]. 

    lo_apre (X) = \bigwedge_{r \in R} \neg \Pre_r (\neg X)

 *)
let lo_apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    
    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref (Mlglu.mdd_one mgr) in

    let not_set = Mlglu.mdd_not set in

    let do_one_rule r : unit =
	let tmp  = pre_rule sp sm not_set r in
	result  := Mlglu.mdd_and !result tmp 1 0;
    in
    Symmod.iter_orules sm do_one_rule;
    Symmod.iter_lrules sm do_one_rule;
    !result
;;


(** Forall-pre-input.
    Given a set [set] of states, we compute the set of states all
    whose input successors are in [set], 
    for the given module [sm] in the symbolic top [sp]. 
    It returns a symbolic representation of the result. 

    Algorithm: 

    Let \phi be the set, and R be the set of input rules of [sm]. 

    i_apre (X) = \bigwedge_{r \in R} \neg \Pre_r (\neg X)

 *)
let i_apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =
    
    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref (Mlglu.mdd_one mgr) in

    let not_set = Mlglu.mdd_not set in

    let do_one_rule r : unit =
	let tmp  = pre_rule sp sm not_set r in
	result  := Mlglu.mdd_and !result tmp 1 0;
    in
    Symmod.iter_irules sm do_one_rule;
    !result
;;

(** This is an alternative version of \forall Pre^O(X) 
 *)
let lo_apre_alt_1 (sp:Symprog.t) (sm:Symmod.t) (tauAll:stateset_t) (x:stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    (* CHECK: all vars are primed; is there a better way?. *)
    let vAll = Symmod.get_vars sm in
    let vAll' = Symprog.prime_vars sp vAll in
    let x' =  Symutil.prime_mdd_vars sp x vAll in
    let implication = Mlglu.mdd_or tauAll x' 0 1 in
    let exists vars (expr:Mlglu.mdd) =
	if (VarSet.is_empty vars) then expr
	else (Mlglu.mdd_smooth mgr expr vars)
    in
    let forall vars expr = Mlglu.mdd_not (exists vars (Mlglu.mdd_not expr)) in
    let finalTerm = forall vAll' implication in
    finalTerm
;;


(** Winning set of a safety game.
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

    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre = lo_apre sp sm !result in
	let contains = Mlglu.mdd_or !result apre 0 1 in
	if Mlglu.mdd_is_one contains then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre 1 1;
    done;
    !result

(** Winning set of a safety game.
    Given a set [set] of states and a module [sm], this function
    computes the set of states from which the Input player has
    a strategy to stay in the set [set].
    It returns a symbolic representation of the result.
    
    Algorithm:
    
    1. Complement set
    2. Enlarge the complement, using lo_pre_star
    3. Complement the result. 

    REMARKS: [set] should be included in both invariants.
             To win a safety game, it's better not to move. 
 *)

let win_i_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    Mlglu.mdd_not (lo_pre_star sp sm (Mlglu.mdd_not set))


(** Winning set of a safety game.
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

    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre = i_apre sp sm !result in
	let contains = Mlglu.mdd_or !result apre 0 1 in
	if Mlglu.mdd_is_one contains then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre 1 1;
    done;
    !result

let win_lo_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    Mlglu.mdd_not (i_pre_star sp sm (Mlglu.mdd_not set))


(** Post for a rule. 
    This function computes the set of states that can be reached from
    a set [set] by one application of the rule [r].
    [sm] is the module for which this is done, and [sp] is the 
    symbolic program top.
    [set] is a predicate over _unprimed_ variables.
    the result is a symbolic representation of the set of states 
    that can be reached.
 *)

let post_rule (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t)
	(r: Symmod.rule_t) : stateset_t =

    (*Printf.printf "applying rule %s\n" (Symmod.get_rule_act r);*)

    let mgr = Symprog.get_mgr sp in 
    
    let w  = Symmod.get_rule_wvars r in 
    let w' = Symprog.prime_vars sp w in 
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
	  let oinv = Symmod.get_oinv sm in
	  Mlglu.mdd_and unprimed_result oinv 1 1
	      
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
	  let iinv = Symmod.get_iinv sm in
	  Mlglu.mdd_and unprimed_result iinv 1 1


(** Takes the local/output post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local and output rules of a module [sm].
    [set] is a predicat over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached.

 *)
let lo_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    let result = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule (r: Symmod.rule_t) : unit =
	let dest = post_rule sp sm set r in 
	result := Mlglu.mdd_or !result dest 1 1
    in
    Symmod.iter_lrules sm do_one_rule;
    Symmod.iter_orules sm do_one_rule;
    !result


(** Takes the input post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local and output rules of a module [sm].
    [set] is a predicat over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached

 *)
let i_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    let mgr = Symprog.get_mgr sp in
    let result = ref (Mlglu.mdd_zero mgr) in

    let do_one_rule (r: Symmod.rule_t) : unit =
	let dest = post_rule sp sm set r in 
	result := Mlglu.mdd_or !result dest 1 1
    in
    Symmod.iter_irules sm do_one_rule;
    !result

(** This does both i_post and lo_post *)
let loi_post (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) : stateset_t =
    let lo_succ  = lo_post sp sm set in 
    let i_succ = i_post sp sm set in 
    Mlglu.mdd_or lo_succ i_succ 1 1 

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
let reachable  (sp: Symprog.t) (sm: Symmod.t) = 
    match sm.reachset with 
	Some r -> r 
      | None -> begin
	    let r = reach sp sm sm.init in 
	    sm.reachset <- Some r; 
	    r
	end


(* **************************************************************** *)
(*                                                                  *)
(*  Builds the symolic representation of a module                   *)
(*                                                                  *)
(* **************************************************************** *)

(** Given the name of a module, 
    it builds and returns the symbolic representation of the module. 
    It takes care of strengthening output and input invariants, and 
    conjoining them to the initial condition. 
*)

let mk_sym (mod_name: string) =
    let mgr = Symprog.get_mgr Symprog.toplevel in
    let m = 
	try Prog.get_mod_top mod_name
	with Not_found ->
	    Printf.printf " Error: Unknown module!";
	    raise Not_found
    in
    (* builds the symbolic module *)
    let sm = Symbuild.mk_mod m Symprog.toplevel in
    (* strengthen invariants to put module into normal form *)
    let strong_iinv = win_i_safe  Symprog.toplevel sm (Symmod.get_iinv sm)
    and strong_oinv = win_lo_safe  Symprog.toplevel sm (Symmod.get_oinv sm) in
    Symmod.set_iinv sm strong_iinv;
    Symmod.set_oinv sm strong_oinv;
    let new_init = Mlglu.mdd_and (Mlglu.mdd_and (Symmod.get_init sm)
	strong_iinv 1 1) strong_oinv 1 1 in 
    Symmod.set_init sm new_init; 
    if Mlglu.mdd_is_zero new_init then begin
	Printf.printf "The module %s has an empty set of initial states!" mod_name; 
	flush stdout
    end; 
    (* adds it to the set of all symbolic modules *)
    Symprog.add_mod_top sm;
    sm



