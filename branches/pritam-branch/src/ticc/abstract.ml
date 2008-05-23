(* This module defines all functions which can be used to abstract implement
 three valued abstraction *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;
open Ops;;

module VarSet = Vset.VS;;
    

(* compute abstract transition relation 
   is invoked intially and after each refinement 
*)
 let compute_abstract_trans (m:Symmod.t)(player1 : bool) (av : VarSet.t) (cv:VarSet.t) : Mlglu.mdd list * Mlglu.mdd list=
  let sp  =  Symprog.toplevel in
  let mgr = Symprog.get_mgr sp in 
  let result1 = ref([]) in
  let result2 = ref([]) in
  let cv' = Symprog.prime_vars sp cv in
  let do_one_rule r : unit =
    let (a,_) = Symmod.get_rule_tran_as_pair r in
     (* abstract transition rule*) 
    let tmp1 =  Mlglu.mdd_smooth mgr a cv' in
    let tmp2m = Mlglu.mdd_smooth mgr tmp1 cv in
    let tmp2M = Mlglu.mdd_consensus mgr tmp1 cv in  
    result1 := List.append [(tmp2m)] !result1;
    result2 := List.append [(tmp2M)] !result2 
  in
  Symmod.iter_orules m do_one_rule; 
  Symmod.iter_lrules m do_one_rule;
  !result1, !result2 ;;

(* Algorithm for fixed point iterations 
   Inputs the initial states, abstract cpre operators, winning objective
   returns abstract winning states
*)
 let find_winning_states trans goal av unclist: Mlglu.mdd = 
   let sp  =  Symprog.toplevel in
   let mgr = Symprog.get_mgr sp in
   let result = ref goal in
   let frontier = ref goal in
   let av' = Symprog.prime_vars sp av in
   while not (Mlglu.mdd_is_zero !frontier) do
     let set' = Symutil.prime_mdd_vars sp !frontier av in
     let preimage = ref (Mlglu.mdd_zero mgr) in
     let compute_preimage_rule r unc: unit =
       let setr = Mlglu.mdd_and r set' 1 1 in
       let setu = Mlglu.mdd_and unc setr 1 1 in
       preimage := Mlglu.mdd_or !preimage setu 1 1    
     in
     List.iter2 compute_preimage_rule trans unclist;    
     preimage := Mlglu.mdd_smooth mgr !preimage av';
     frontier := Mlglu.mdd_and !preimage !result 1 0;  
     result := Mlglu.mdd_or !result !preimage 1 1  
   done;
   !result;; 

(* Given a list of rules and a set of concrete variables,
   finds out rules where maybeimage is true in the guards 
   in those rules find the concrete variables*)
   (* another heuristic is to add the varible which appears in most number of maybe rules*)
 let find_significant_var cv maybeimage m mgr : VarSet.t =
   let vs = ref (VarSet.empty) in
   let one_rule_vars r : unit =
     let (a,_) = Symmod.get_rule_tran_as_pair r in
     let imtrans = Mlglu.mdd_and maybeimage a 1 1 in
     let newc = if (Mlglu.mdd_is_zero imtrans) then VarSet.empty 
                else VarSet.inter cv (Mlglu.mdd_get_support mgr a) in
      vs := VarSet.union !vs newc
   in
   Symmod.iter_orules m one_rule_vars; 
   Symmod.iter_lrules m one_rule_vars;
   !vs;;

let update_unchngd_list sp wvarlist c unclist : Mlglu.mdd list =
  let mgr = Symprog.get_mgr sp in
  let do_one_rule unc wvar : Mlglu.mdd =
    if (VarSet.mem c wvar) then unc
    else
    let c'  = Symprog.prime_id sp c in
    let cc' = Mlglu.mdd_eq mgr c c' in
    Mlglu.mdd_and unc cc' 1 1
  in 
  List.map2 do_one_rule unclist wvarlist;;

(* Algorithm for refinement given two set of winning states
   (initially, do split based on some given order of the list)
*)
 
 let split winm winM may_trans av cv m unclist: Vset.varid_t =
   let sp  =  Symprog.toplevel in
   let mgr = Symprog.get_mgr sp in 
   let av' = Symprog.prime_vars sp av in
   let set' = Symutil.prime_mdd_vars sp winM av in
   let mayand = ref (Mlglu.mdd_zero mgr) in
   let compute_one_rule r unc: unit =
     let uncr = Mlglu.mdd_and r unc 1 1 in
     mayand := Mlglu.mdd_or !mayand (Mlglu.mdd_and uncr set' 1 1) 1 1
   in  
   List.iter2 compute_one_rule may_trans unclist;
   let preimage = Mlglu.mdd_smooth mgr !mayand av' in
   let maybeimage = Mlglu.mdd_and preimage (Mlglu.mdd_and winm winM 1 0) 1 1 in 
   (*Mlglu.mdd_print mgr maybeimage;*)
   let vs = find_significant_var cv maybeimage m mgr in
   (*Symutil.print_varset sp vs; *)
   let cvl = Vset.to_list vs in
   List.hd cvl
;;


let create_unchngd_list sp wvarlist av : Mlglu.mdd list =
  let mgr = Symprog.get_mgr sp in 
  let do_one_rule wvars : Mlglu.mdd = 
    let uncvars = VarSet.diff av wvars in 
    Symutil.and_unchngd sp (Mlglu.mdd_one mgr) uncvars 
  in 
  List.map do_one_rule wvarlist
;;

let get_wvar_list m : VarSet.t list = 
  let wvarlist = ref [] in
  let one_rule_vars r : unit =
    wvarlist := List.append [(Symmod.get_rule_wvars r)] !wvarlist
  in 
  Symmod.iter_orules m one_rule_vars; 
  Symmod.iter_lrules m one_rule_vars;
  !wvarlist;;
  

   
(* The main algorithm to compute the final winning states 
   The main loop which calls the abstract fixpoint iterations
   and refienement functions
   The main loop continues to refine the states in between the two winning
   abstract regions and go back computing the abstract winning states
*)
let ctl_e_f m i goal : bool =
   let sp  =  Symprog.toplevel in
   let mgr = Symprog.get_mgr sp in 
   let av = ref (VarSet.union (Mlglu.mdd_get_support mgr goal) (Mlglu.mdd_get_support mgr i)) in
   let wvarlist = get_wvar_list m in
   let unclist = ref (create_unchngd_list sp wvarlist !av) in
   (*let av = ref (Symmod.get_vars m) in *)
   let result = ref 2 in
   (while !result = 2 do
    let start = Sys.time () in
    let cv =  VarSet.diff (Symmod.get_vars m) !av in
    let (may_trans, must_trans) = compute_abstract_trans m true !av cv in
    let aftertrans = Sys.time () in
    let init = Mlglu.mdd_smooth mgr i cv in
    let winm = find_winning_states may_trans goal !av !unclist in
    let winM =  find_winning_states must_trans goal !av !unclist in
    let afterwinning = Sys.time () in
    result := if not (Mlglu.mdd_is_zero (Mlglu.mdd_and winM init 1 1)) then 1
              else if (Mlglu.mdd_is_zero (Mlglu.mdd_and winm init 1 1)) then 0
              else (
		let newv = split winm winM may_trans !av cv m !unclist in
                av :=  VarSet.add newv !av; 
                unclist := update_unchngd_list sp wvarlist newv !unclist; 2);
   let finish = Sys.time () in
   Printf.printf "\n trans build %f winning %f splitting %f \n" (aftertrans -. start) (afterwinning -. aftertrans) (finish -. afterwinning);
    done); 
  (* Symutil.print_varset sp (VarSet.diff (Symmod.get_vars m) !av);   *)
   if (!result = 1) then true else false     
;;
(* issues to do
   1. implement the reachability, the safety, cobuchi, timed first
   2. do for general n 
   3. algorithm to find a suitable variable (when not provided by the user)
   4. need to work with list of actions/transitions instead of and/or-ing them
   5. in timed game, first move by output and second by input and  we
      are interested to compute winning set for the input (i.e. 2nd player) 
*)

