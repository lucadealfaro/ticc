(** ****************************************************************
    Print functions for symbolic modules. 
 *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;; 

type stateset_t = Mlglu.mdd;;
type relationset_t = Mlglu.mdd;;
module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 

exception Bad_typ;;

(** Prints a (symbolic) rule. *)

let print_rule sp r : unit =
    if true then 
	begin
	    let mgr = Symprog.get_mgr sp in
	    Printf.printf "action %s:--------\n" (Symmod.get_rule_act r);
	    Printf.printf "Modifies only variables in: ";
            Symutil.print_varset sp (Symmod.get_rule_wvars r);
	    Printf.printf "\n";
	    let tran = Symmod.get_rule_tran r in
	    match Symmod.get_rule_tran r with
		Symmod.Loc m  -> 
		    Printf.printf "----[local]:\n"; 
		    Printf.printf "Owned by module %s\n" (Symmod.get_owner_module r); 
		    flush stdout;
		    Mlglu.mdd_print mgr m;
	      | Symmod.Out m -> 
		    Printf.printf "----[output]:\n";
		    Printf.printf "Owned by module %s\n" (Symmod.get_owner_module r); 
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
    let print_r r = 
	if act = Symmod.get_rule_act r then 
	    match Symmod.get_rule_tran r with
	      Symmod.Loc m  -> 
		  Printf.printf "[local part]:\n"; 
		  Printf.printf "Owned by module %s\n" (Symmod.get_owner_module r); 
		  Printf.printf "modified vars:\n ";
		  Symutil.print_varset sp (Symmod.get_rule_wvars r);
		  Printf.printf "\n \n";
		  flush stdout;
		  Mlglu.mdd_print mgr m;
		  Printf.printf "\n\n";
            | Symmod.Out m -> 
		  Printf.printf "[output part]:\n\n";
		  Printf.printf "Owned by module %s\n" (Symmod.get_owner_module r); 
		  Printf.printf "modified vars:\n ";
		  Symutil.print_varset sp (Symmod.get_rule_wvars r);
		  Printf.printf "\n \n";
		  flush stdout;
		  Mlglu.mdd_print mgr m;
		  Printf.printf "\n\n";
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
    Symmod.iter_irules sm print_r; 
    Symmod.iter_lrules sm print_r; 
    Symmod.iter_orules sm print_r; 
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
    Printf.printf "* All variables:";
    Symutil.print_varset sp (Symmod.get_vars sm);
    Printf.printf "\n \n";
    Printf.printf "* History variables: ";
    Symutil.print_varset sp (Symmod.get_hvars sm);
    Printf.printf "\n \n";
    Printf.printf "* Local variables: ";
    Symutil.print_varset sp (Symmod.get_lvars sm);
    Printf.printf "\n \n";
    Printf.printf "* Global variables: ";
    Symutil.print_varset sp (Symmod.get_gvars sm);
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
    input invariant, in a symbolic module [sm]. *)
let print_input_restriction sp sm (r: string) = 
    flush stdout; 
    let mgr = Symprog.get_mgr sp in
    let iinv = Symmod.get_iinv sm in
    if Symmod.has_iaction sm r then begin 
      Printf.printf "\n------------------------"; 
      Printf.printf "\nRestriction of input action %s:\n" r; 
      let ir = Symmod.get_irule sm r in 
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
      (* Computes the set of reachable states of the module *)
      let reachset = Ops.reachable sp sm in 
      (* Now builds the answer: restr = tr /\ iinv /\ reachset /\ not iinv' *)
      let iinv' = Symutil.prime_mdd sp sm iinv in
      let restr = Mlglu.mdd_and 
	(Mlglu.mdd_and (Mlglu.mdd_and tr iinv 1 1) reachset 1 1)
	iinv' 1 0 in 
      (* At this point, what is left to do is to quantify out
	 all primed local variables, since anyway their update
	 is deterministic. *) 
      let local_variables = Symmod.get_lvars sm in 
      let local_variables' = Symprog.prime_vars sp local_variables in
      let result = Mlglu.mdd_smooth mgr restr local_variables' in
      flush stdout; 
      Mlglu.mdd_print mgr result;
      flush stdout; 
      Printf.printf "\n------------- end of restriction\n" 
    end
    else 
      Printf.printf "\nNo input action named %s in the module.\n" r
;;
