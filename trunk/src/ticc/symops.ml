(** This module contains operations on ticc modules, implemented 
    symbolically. *)

open Str;;
open Symmod;;
open Ticc;;

type stateset_t = Mlglu.mdd;;
type relationset_t = Mlglu.mdd;;

module VarSet = Vset.VS;;

type varid_t = Vset.varid_t;; 

open Ast;;

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

(** ****************************************************************
    Print functions
 *)

(** Prints a (symbolic) rule. *)

let print_rule sp r : unit =
    if (Symmod.get_rule_act r) != Symprog.env_act then 
	begin
	    let mgr = Symprog.get_mgr sp in
	    Printf.printf "action %s:--------\n" (Symmod.get_rule_act r);
	    (* Printf.printf "    modified vars: ";
               Symutil.print_varset sp (Symmod.get_rule_wvars r); *)
	    let tran = Symmod.get_rule_tran r in
	    match Symmod.get_rule_tran r with
		Symmod.Loc m  -> 
		    Printf.printf "----[local]:\n"; 
		    flush stdout;
		    Mlglu.mdd_print mgr m;
	      | Symmod.Out m -> 
		    Printf.printf "----[output]:\n";
		    flush stdout;
		    Mlglu.mdd_print mgr m; 
	      | Symmod.Inp (mg, ml) -> 
		    Printf.printf "----[input global]:\n";
		    flush stdout;
		    Mlglu.mdd_print mgr mg; 
		    Printf.printf "----[input local]:\n";
		    flush stdout;
		    Mlglu.mdd_print mgr ml;
		    Printf.printf "\n\n";
		    
	end
;;


(** print_rulemod. *)

let print_rulemod sp sm typ: unit = 
    let mgr = Symprog.get_mgr sp in
    Printf.printf "\n";
    Printf.printf "   PRINTING %s rules of SYMBOLIC MODULE: %s.\n" 
	typ (Symmod.get_name sm);
    Printf.printf "\n";
    match typ with
	"Input" ->    Symmod.iter_irules sm (print_rule sp)
      | "Output" -> Symmod.iter_orules sm (print_rule sp)
      | "Local"  -> Symmod.iter_lrules sm (print_rule sp)
      | _  -> raise Bad_typ

;;

(** Prints the Input/Output/Local part of 
    a rule [act] that belongs to a module [sm].
    Notice that the rules returns an empty list
    if [act] does not belong to [sm].
 *)


let print_rulemodact sp sm act : unit =
    Printf.printf "\n";
    Printf.printf "   PRINTING the rule(s) for the action %s of SYMBOLIC MODULE: %s.\n\n"  act (Symmod.get_name sm);
    let mgr = Symprog.get_mgr sp in
    let rules_act = get_rules sm act in
    let rec print_rules_act rules_list = 
	match rules_list with
	    [] ->  ()
	  | r::rs -> match Symmod.get_rule_tran r with
		Symmod.Loc m  -> 
	            Printf.printf "[local part]:\n"; 
                    Printf.printf "modified vars:\n ";
                    Symutil.print_varset sp (Symmod.get_rule_wvars r);
                    Printf.printf "\n \n";
	            flush stdout;
	            Mlglu.mdd_print mgr m;
                    Printf.printf "\n\n";
                    print_rules_act rs;
              | Symmod.Out m -> 
	            Printf.printf "[output part]:\n\n";
                    Printf.printf "modified vars:\n ";
                    Symutil.print_varset sp (Symmod.get_rule_wvars r);
                    Printf.printf "\n \n";
	            flush stdout;
	            Mlglu.mdd_print mgr m;
                    Printf.printf "\n\n";
                    print_rules_act rs;
              | Symmod.Inp (mg, ml) -> 
                    Printf.printf "[input part]:\n";
                    Printf.printf "modified vars:\n ";
                    Symutil.print_varset sp (Symmod.get_rule_wvars r);
                    Printf.printf "\n \n";
	            Printf.printf "[input global part]:\n";
                    flush stdout;
	            Mlglu.mdd_print mgr mg; 
                    Printf.printf "\n";
	            Printf.printf "[input local part]:\n";
	            flush stdout;
	            Mlglu.mdd_print mgr ml;
	            Printf.printf "\n\n"
    in
    print_rules_act rules_act               
;;


(** Prints the statesets of a symbolic module *)
let print_statesets (sm: Symmod.t) mgr : unit = 
  let print_set name bdd = 
    Printf.printf "   %s: " name; 
    flush stdout;
    Mlglu.mdd_print mgr bdd;
    Printf.printf "\n"
  in
  Symmod.iter_ssets sm print_set 

(** Prints the contents of the symbolic module [sm]. *)

let print sp sm : unit =
    let mgr = Symprog.get_mgr sp in
    Printf.printf "\n===================================\n";
    Printf.printf "Printing symbolic module %s.\n" (Symmod.get_name sm);
    Printf.printf "* Known variables:";
    Symutil.print_varset sp (Symmod.get_vars sm);
    Printf.printf "\n \n";
    Printf.printf "* History variables: ";
    Symutil.print_varset sp (Symmod.get_hvars sm);
    Printf.printf "\n \n";
    Printf.printf "* Input Invariant:\n";
    flush stdout;
    Mlglu.mdd_print mgr (Symmod.get_iinv sm);
    Printf.printf "\n";
    Printf.printf "* Output Invariant:\n";
    flush stdout;
    Mlglu.mdd_print mgr (Symmod.get_oinv sm);
    Printf.printf "\n";
    Printf.printf "* Initial Condition:\n";
    flush stdout;
    Mlglu.mdd_print mgr (Symmod.get_init sm); 
    (* prints the statesets, if any *)
    if (Hsetmap.size (Symmod.get_ssets sm)) > 0 then begin
      Printf.printf "* Statesets:\n";
      flush stdout;
      print_statesets sm mgr
    end; 
    Printf.printf "* Rules:\n\n";
    Symmod.iter_lrules sm (print_rule sp);
    Symmod.iter_orules sm (print_rule sp);
    Symmod.iter_irules sm (print_rule sp);
    Printf.printf "endmodule";
    Printf.printf "\n===================================\n\n";
    flush stdout

(** Prints how an input action [r] has become restricted due to the 
    input invariant, in a symbolic module [sm]. Luca *)
let print_input_restriction sp sm (r: string) = 
    flush stdout; 
    Printf.printf "\n------------------------"; 
    Printf.printf "\nRestriction of input action %s:\n" r; 
    let mgr = Symprog.get_mgr sp in
    let iinv = Symmod.get_iinv sm in
    let r = Symmod.get_irule sm r in 
    match r with 
	None -> Printf.printf "\nNo such action."
      | Some ir -> begin
	    let (glob_tr, loc_tr) = Symmod.get_rule_tran_as_pair ir in
	    (* The transition relation is tr = glob_tr /\ loc_tr /\ uncha, 
	       where uncha is an assertion saying that the local
	       variables that are not mentioned do not change their
	       value. *) 
	    let w = Symmod.get_rule_wvars ir in
	    let alllocal = Symmod.get_lvars sm in
	    let notw = VarSet.diff alllocal w in
	    let unchange_local = Symbuild.unchngd sp notw in
	    let tr_tmp = Mlglu.mdd_and glob_tr loc_tr 1 1 in 
	    let tr = Mlglu.mdd_and tr_tmp unchange_local 1 1 in
	    (* Now builds the answer: restr = tr /\ iinv /\ not iinv *)
	    let iinv' = Symutil.prime_mdd sp sm iinv in
	    let violate_tr = Mlglu.mdd_and iinv iinv' 1 0 in 
	    let restr = Mlglu.mdd_and tr violate_tr 1 1 in
	    (* At this point, what is left to do is to quantify out
	       all primed local variables, since anyway their update
	       is deterministic. *) 
	    let local_variables = Symmod.get_lvars sm in 
	    let local_variables' = Symprog.prime_vars sp local_variables in
	    let result = Mlglu.mdd_smooth mgr restr local_variables' in
	    flush stdout; 
	    Mlglu.mdd_print mgr result;
	    flush stdout
	end;
	    Printf.printf "\n------------- end of restriction\n" 
;;


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
          let oinv = Symmod.get_oinv sm in
	  let valid_set = Mlglu.mdd_and set oinv 1 1 in
	  (* set' = valid_set [w' / w] *)
	  let set' = Symutil.prime_mdd_vars sp valid_set w in 
	  (* the result is \exists W' . (\tau /\ set') *) 
	  Mlglu.mdd_and_smooth mgr tau set' w' 
      | Symmod.Inp (taug, taul) -> 
	  (* conjoin [set] with the input invariant,
             as the Input player cannot escape it *)
          let iinv = Symmod.get_iinv sm in
	  let valid_set = Mlglu.mdd_and set iinv 1 1 in
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
	    
	    
(** Forall-pre-output-local.
    Given a set [set] of states, we compute the set of states all
    whose output/local successors are in [set], 
    for the given module [sm] in the symbolic top [sp]. 
    It returns a symbolic representation of the result. 

    Algorithm: 

    Let \phi be the set, and R be the set of output or local rules of [sm]. 

    lo_apre (X) = \bigwedge_{r \in R} \neg \Pre_r (\neg X)

 *)
let lo_apre (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =
    
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

(** Implement version #1 of \forall Pre^O(X) 
 *)
let lo_apre3 (sp:Symprog.t) (sm:Symmod.t) (tauAll:stateset_t) (x:stateset_t) : stateset_t =
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
let win_i_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =

    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre = lo_apre sp sm !result in
	let contains = Mlglu.mdd_or !result apre 0 1 in
	if Mlglu.mdd_is_tautology contains 1 then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre 1 1;
    done;
    !result


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
let win_lo_safe (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
	: stateset_t =

    let mgr = Symprog.get_mgr sp in
    (* start with "true" *)
    let result = ref set in
    let finished = ref false in 
    (* repeat until result => apre *)
    while (not !finished) do
	let apre = i_apre sp sm !result in
	let contains = Mlglu.mdd_or !result apre 0 1 in
	if Mlglu.mdd_is_tautology contains 1 then
	    finished := true
	else
	    result := Mlglu.mdd_and !result apre 1 1;
    done;
    !result


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
	     ( (\exists allvars . 
	        (\tau /\ set)[notw'/notw] )[allvars/allvars'] ) /\ oinv
	  *)
	    
	  let notw = VarSet.diff allvars w in 
	  let set_tau = Mlglu.mdd_and set tau 1 1 in 
	  let set_tau_notw' =  Symutil.prime_mdd_vars sp set_tau notw in
	  let result  = Mlglu.mdd_smooth mgr set_tau_notw' allvars in
	  let unprimed_result = Symutil.unprime_mdd_vars sp result allvars' in
	  let oinv = Symmod.get_oinv sm in
	  Mlglu.mdd_and unprimed_result oinv 1 1
	      
      | Symmod.Inp (taug, taul) -> 
	  
	  (* the result is
	     ( (\exists allvars . 
	        (taug /\ taul /\ set)[(V^l-w)'/(V^l-w)]] )[allvars/allvars'] ) /\ iinv
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
;;

(** Takes the local/output post of a set of states [set]. 
    This function computes the set of states that can be reached
    from a set [set] by application of the union of all
    local and output rules of a module [sm].
    [set] is a predicat over _unprimed_ variables.
    The result is a symbolic representation of the set of states
    that can be reached

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


(** **************** Composition of modules  ******************* *)

(** Search through a list of rules for a particular type. 
    This function takes a list of rules [rule_list] and searches
    if one of them is of type [typ].
    In the positive case, the function returns the corresponding
    rule. Otherwise, it returns a rule whose body is FALSE.

 *)
let rec find_rule_or_false (sp:Symprog.t) act typ rule_list =
    match rule_list with
	[]	->
	    (* if there is no rule for an action, then the
	     * model cannot accept that action.  Set the mdd
	     * to "false".  I pass a list of two, because mk_rule
	     * will do the correct thing, based on the rule type. *)
	    let mgr = Symprog.get_mgr sp in
	    Symmod.mk_rule typ act VarSet.empty
		[(Mlglu.mdd_zero mgr); (Mlglu.mdd_zero mgr)]
      | r::rs	->
	    if (Symmod.get_rule_type r) = typ then r
	    else find_rule_or_false sp act typ rs
;;


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
    
    \forall a \in \Actions_2 . 
    W_2(a) \cap \vstate{1} \neq \emptyset \implies a \in \Actions_1 
    \forall a \in \Actions_1 .
    W_1(a) \cap \vstate{2} \neq \emptyset \implies a \in \Actions_2. 

    and
    
    V_1^L \cap V_2^L {\equal} {\empty}

    and

    local actions of both modules are not shared.

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
    let interferes wvars v = not(VarSet.is_empty (VarSet.inter wvars v)) in
    let check_for_interference sm1 sm2 sm2_v (r: rule_t)=
	if !result then begin
	    (* if result is false, we already know that [m1] and [m2] 
	       are not composable*)
	    let act = Symmod.get_rule_act r in
	    let wvars = Symmod.get_rule_wvars r in
	    if (interferes wvars sm2_v)
		&& not (Symmod.has_action sm2 act)
	    then 
	    begin  
	    Printf.printf "Modules %s and %s have a composability problem.\n" 
            (Symmod.get_name sm1) (Symmod.get_name sm2); 
	    flush stdout;
            Printf.printf "Action %s of Module %s modifies variables that are also part of Varset of module %s. The set of variables that is modified is " act (Symmod.get_name sm1) (Symmod.get_name sm2);
	    flush stdout;
	    Symutil.print_varset sp (VarSet.inter wvars sm2_v);
	    Printf.printf ".\n";
	    Printf.printf "However, Module %s do not have action %s\n \n" 
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

(** product. 
    This function computes the product of two modules [m1] 
    and [m2]. The order is given by [m1] product [m2].
    
    The function takes an argument [result_name] that can
    be used to name the result of the product. 

    NEED TO BE DONE:

    - fixing the clock bounds.
 *)
let product (sp:Symprog.t) ?(result_name="") (m1:Symmod.t) (m2:Symmod.t)
	: Symmod.t =
    let mgr = Symprog.get_mgr sp in
    (*if the user does not choose a name, the concatenation
      of the names of the two modules is taken*)
    let sm_name =
	if result_name = ""
	then (Symmod.get_name m1) ^ "*" ^ (Symmod.get_name m2)
	else result_name
    in
    let new_sm = Symmod.mk mgr sm_name in
    let lvars1 = Symmod.get_lvars m1 in
    let lvars2 = Symmod.get_lvars m2 in

    (* this function adds a new rule in the product.
       The function is called when the action that label
       the rule [r] is not shared by the module [other_sm].
     *)

    let merge_unshared_action (other_sm:Symmod.t) (r:Symmod.rule_t) : unit = 
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
		    (* the global variables of the other module cannot
		       change *) 
		    let new_mdd_ig = Symbuild.and_same sp mdd_ig other_hvars in
		    (* for the local part, wvars keeps track of which
		       variables can change, so there is no need to
		       change the transition relation. *) 
		    let new_mdd_il = mdd_il in
		    let new_wvars = r.wvars in
		    Symmod.mk_irule (Symmod.get_rule_act r)
			new_wvars mdd_ig mdd_il
	in
	add_rule new_sm new_rule
    in

    (*
      This function adds a new rule in the product.
      The function is called when two modules
      share a common action [act].
      The action [act] is linked to the list
      of rules [m1_rules] in the first module,
      and to the list [m2_rules] in the second module.
     *)
    let merge_shared_action act (m1_rules:rule_t list) (m2_rules:rule_t list) =
	(*
	  This is just a sanity check, to check that there is no local
	  rule among the shared ones.  
	  The check is not that useful, as it is already done by the
	  composability test, but just in case... 
	 *)
	let rec verify_no_lrule(rule_list:rule_t list) = 
	    let is_loc r = 
		if get_rule_type r = Local 
		then raise Shared_Local_Rule
	    in
	    List.iter is_loc rule_list 
	in
	verify_no_lrule m1_rules;
	verify_no_lrule m2_rules;

	(*
          the following 8 lines of code are used to extract
          the bdds corresponding to the input and the outputs
          of the two modules. The body of a rule is FALSE
          if the module does not contain a rule of the required type
          (I/O) for the given action. 
	 *)
	let inp_m1 = find_rule_or_false sp act Input m1_rules in
	let inp_m2 = find_rule_or_false sp act Input m2_rules in
	let out_m1 = find_rule_or_false sp act Output m1_rules in
	let out_m2 = find_rule_or_false sp act Output m2_rules in
	(* gets the MDDs for the global and local parts of a rule *) 
	let (rho1_ig, rho1_il) = Symmod.get_rule_ig_il_mdds inp_m1 in
	let (rho2_ig, rho2_il) = Symmod.get_rule_ig_il_mdds inp_m2 in
	(* gets the MDDs for the output transition relation *) 
	let rho1_o   = Symmod.get_orule_mdds out_m1 in
	let rho2_o   = Symmod.get_orule_mdds out_m2 in
	(* gets the wvars *) 
	let o_wvars1 = Symmod.get_rule_wvars out_m1 in
	let o_wvars2 = Symmod.get_rule_wvars out_m2 in
	let i_wvars1 = Symmod.get_rule_wvars inp_m1 in 
	let i_wvars2 = Symmod.get_rule_wvars inp_m2 in 
	(* ... and computes the ones of the new merged rule *) 
	let o_wvars12 = VarSet.union o_wvars1 o_wvars2 in
	let i_wvars12 = VarSet.union i_wvars1 i_wvars2 in 
	let io_wvars  = VarSet.union o_wvars12 i_wvars12 in 
	(* computes the transition relation for the new merged input
           rule. note that for the local part, also i_wvars12 is relevant *) 
	let rho12_il = Mlglu.mdd_and rho1_il rho2_il 1 1 in
	let rho12_ig = Mlglu.mdd_and rho1_ig rho2_ig 1 1 in
	(*
	  In the following lines, we define rho12_o that 
	  describe how an output of the product is obtained by synchronization
	  of a local input of a module and an output of the other one.
	  We recall that this new rule is a disjunction whose terms depend
          on which module owns the output.
	  
	  [unchgnd_1] is an MDD that says that the variables in 
	  the set of terms that must stay unchanged in the first
	  part of the disjunction, and [unchgnd_2] is the set for the second 
	  part. 
	 *)
	let unchgnd_1 = Symbuild.unchngd sp
	    (VarSet.union
                (* These are the local variables which do not change their value: the
		   local variables of 1 (because we take here the local trans rel of
		   2), minus the variables that appear as output in 1, since an output
		   transition relation may also contain local variables.  Luca and Bo
		   checked this, originally written by Marco and Axel, and agree, so
		   it must be correct :-) *) 
		(VarSet.diff i_wvars1 o_wvars1)
		(* These are the global variables that keep their
		   value: the ones that are controlled by module 2,
		   and not changed by module 1. *) 
		(VarSet.diff o_wvars2 o_wvars1)) in
	let part1 = Symbuild.conj_mdd_list mgr rho1_o [rho2_il; unchgnd_1] in 

	let unchgnd_2 = Symbuild.unchngd sp
	    (VarSet.union
		(VarSet.diff i_wvars2 o_wvars2)
		(VarSet.diff o_wvars1 o_wvars2)) in
	let part2 = Symbuild.conj_mdd_list mgr rho2_o [rho1_il; unchgnd_2] in 
	
	(* rho12_o define the output that is obtained from the conjunction of
           one input with one output *)
	let rho12_o = Mlglu.mdd_or part1 part2 1 1 in 

	(*
	  This rule add in the product an output rule that 
	  is the result of the syncrhonization of the local input
	  of one module with the output of the other one.
	  We define the set of variables that can be written by this rule 
	  as the union of the set of variables that can be rewritten 
	  during the syncrhonization.  
	  
	 *)
	add_rule new_sm (Symmod.mk_orule act io_wvars rho12_o);
	(*
          We add a rule for each input that is obtained by the synchronization
          of an input of each module.
          Notice that the set W associated to the rule
          is the union of the sets W of the two local inputs that have 
          participated to the creation of the new input.
         *)
	add_rule new_sm (Symmod.mk_irule act i_wvars12 rho12_ig rho12_il);
    in

    (* this function receives an action [action] as input.
       The function extracts the rules for [action] in
       modules [m1] and [m2]. Then, depending on the results,
       it constructs a new rule in the product.
       
       Remark

       The function can rises an exception if both rules are false.
       In practice, it should not happen, but it is needed to complete
       the pattern matching process.
     *)

    let merge_action action =
	let m1_rules = get_rules m1 action in
	let m2_rules = get_rules m2 action in
	match (m1_rules, m2_rules) with
	    ([], [])	-> raise Internal_error_1 (* someone must have the act *)
	  | (_,  [])	-> List.iter (merge_unshared_action m2) m1_rules
	  | ([], _)	-> List.iter (merge_unshared_action m1) m2_rules
	  | (_,  _)	-> merge_shared_action action m1_rules m2_rules
    in
    
    (* this function receives a rule [next_rule] as input.
       It first check if there exists a rule
       with the same action name in the product.
       If no, the function calls [merge_action] to handle it.
       If yes, the function does nothing.
       
     *)
    let check_for_merge (next_rule:rule_t) =
	(* merge the actions represented by the rule, but don't
	 * merge if we've already merged. *)
	let action = Symmod.get_rule_act next_rule in
	if not (Symmod.has_action new_sm action) then
	    (* hasn't been merged yet *)
	    merge_action action;
    in
    


    (* the following lines of code are used
       to construct the tuple that defines the new module.
       Particularly, the "iter rules" are used to build 
       the rules of the new module.
       Note that this version follows the product definition (def 15)                  
       given in the FROCOS 05 paper.
     *)
    let vl12 = VarSet.union lvars1 lvars2 in
    let vg12 = VarSet.union (Symmod.get_gvars m1) (Symmod.get_gvars m2) in
    let vh12 = VarSet.union (Symmod.get_hvars m1) (Symmod.get_hvars m2) in
    let vc12 = VarSet.union (Symmod.get_cvars m1) (Symmod.get_cvars m2) in
    (* 5a. TODO: what about clock bounds? *)
    let iinv = Mlglu.mdd_and (Symmod.get_iinv m1) (Symmod.get_iinv m2) 1 1 in
    let oinv = Mlglu.mdd_and (Symmod.get_oinv m1) (Symmod.get_oinv m2) 1 1 in
    VarSet.iter (Symmod.add_var new_sm)
    	(List.fold_left VarSet.union vl12 [ vg12; vh12; vc12 ]);
    VarSet.iter (Symmod.add_lvar new_sm) vl12;
    VarSet.iter (Symmod.add_gvar new_sm) vg12;
    VarSet.iter (Symmod.add_hvar new_sm) vh12;
    (* TODO: VarSet.iter (Symmod.add_cvar new_sm) vc12; *)
    Symmod.set_iinv new_sm iinv;
    Symmod.set_oinv new_sm oinv;

    
    Symmod.iter_lrules m1 check_for_merge;
    Symmod.iter_irules m1 check_for_merge;
    Symmod.iter_orules m1 check_for_merge;
    Symmod.iter_lrules m2 check_for_merge;
    Symmod.iter_irules m2 check_for_merge;
    Symmod.iter_orules m2 check_for_merge;

    new_sm
;;

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

let composition (sp:Symprog.t) win_algo ?(result_name="") (m1:Symmod.t) (m2:Symmod.t)
	: Symmod.t =
    let mgr = Symprog.get_mgr sp in

    (* Verify composability *)
    if not(is_composable sp m1 m2) then raise Modules_not_composable; 

    (* calculate product *)
    let productm1m2 = product sp ~result_name:result_name m1 m2 in

    (*
      This function takes two modules [m1] and [m2].
      For an action [action], it builds on term
      of the conjunction of Defn~15 of the
      FROCOS paper.
     *)

    let build_good_term m1 m2 action = 
        let m1_rules = get_rules m1 action in
	let m2_rules = get_rules m2 action in
	let out_m1 = find_rule_or_false sp action Output m1_rules in
	let inp_m2 = find_rule_or_false sp action Input m2_rules in
	let (rho2_ig,rho2_il) = Symmod.get_rule_tran_as_pair inp_m2 in
	let (rho1_o, _)  = Symmod.get_rule_tran_as_pair out_m1 in
        let wvars_1 = Symmod.get_rule_wvars out_m1 in
        let unchngd_vars = VarSet.diff (Symmod.get_gvars m2) wvars_1 in
	let unchngd_term = Symbuild.unchngd sp unchngd_vars in
	let antecedent = Mlglu.mdd_and rho1_o unchngd_term 1 1 in
	let consequent = rho2_ig in
	let implication = Mlglu.mdd_or antecedent consequent 0 1 in
	let vAll_12' = Symprog.prime_vars sp (Symmod.get_vars productm1m2) in
        let finalTerm = Mlglu.mdd_consensus mgr implication vAll_12' in
        finalTerm in


    (*
      The function build_good_states constructs a predicate that
      represents the set of good states in the product of two modules
      [m1] and [m2].

      Algorithm

      The function is defined to be iterated on the rules of [m1]
      For each rule, the function checks if module [m2] has a
      shared action with this rule. If yes, the function constructs
      the set of good_states that concerns this action.
      This set is then conjoined with the part that has already been
      constructed. At the start, the set is true, supposing that all
      states of the product are good states.
     *)
    let goodStates = ref (Mlglu.mdd_one mgr) in
    let build_good_states (next_rule:rule_t) =
	(* Look for shared actions, and if Input matches output,
	 * then add the predicate to the list of good states.
	 * If the action is not shared, then we don't have to do anything *)
	let action = get_rule_act next_rule in
	if (Symmod.has_action m2 action) then begin
            (* Implement defn~21 *)
            flush stdout;
	    goodStates := Symbuild.conj_mdd_list mgr !goodStates
		[(build_good_term m1 m2 action);
		(build_good_term m2 m1 action)]
        end
    in
    (* get list of good states.  Since only shared actions matter,
     * we only need to search through one module for its actions. *)
    Symmod.iter_irules m1 build_good_states;
    Symmod.iter_orules m1 build_good_states;
    (* the set good_state is then used to restrict
       the input invariant of the product *)
    let iinv_12 = Symmod.get_iinv productm1m2 in
    let safety_area = Mlglu.mdd_and !goodStates iinv_12 1 1 in
    let winning_area = win_algo sp productm1m2 safety_area in
    let iinv_composed = Mlglu.mdd_and iinv_12 winning_area 1 1 in
    Symmod.set_iinv productm1m2 iinv_composed;
    (* 
       After computing the set of good states, we play the game to restrict the 
       Input invariant of the product.
     *)
    let oinv_12 = Symmod.get_oinv productm1m2 in
    let allowed_area = Mlglu.mdd_and iinv_composed oinv_12 1 1 in
    let zero = Mlglu.mdd_zero mgr in
    if (Mlglu.mdd_equal allowed_area zero) then raise Incompatible_Modules;
    (* and return the composition *)
    productm1m2
;;

(* End of functions needed for composition                                   *)
(*---------------------------------------------------------------------------*)


(* **************************************************************** *)
(*                                                                  *)
(*  Builds the symolic representation of a module                   *)
(*                                                                  *)
(* **************************************************************** *)

(** Given the name of a module, 
    it builds and returns the symbolic representation of the module. 
*)

let mk_sym (mod_name: string) =
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
  (* adds it to the set of all symbolic modules *)
  Symprog.add_mod_top sm;
  sm



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



