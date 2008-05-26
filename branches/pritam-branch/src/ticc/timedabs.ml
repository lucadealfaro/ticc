(* This module defines all functions which can be used to abstract a timed game*)
open Str;;
open Symmod;;
open Ticc;;
open Ast;;
open Ops;;
open Vset;;

module VarSet = Vset.VS;;

let print_mdd s mgr x : unit =
        
        (* Printf.printf " \n"; flush stdout;
         print_string s;
         Printf.printf " \n"; flush stdout;
	 Mlglu.mdd_print mgr x;
	 Printf.printf " end x \n";*)
 flush stdout;;

(** If [input_first] is true, returns the transition relations 
  of the turn-based game where input plays before output.
  Otherwise, returns the transition relations 
  of the turn-based game where output plays before input.

 The transition relations are lists of mdds and two flags whether need to 
 conjoin with copy_x_x' , copy_x_x'' respectively.
 The disjunction of list items actually give the transition relation.
 There are two lists for two players.
 *)
let turn_transitions sp sm (input_first: bool) : (Mlglu.mdd * bool * bool) list * (Mlglu.mdd * bool * bool) list * Mlglu.mdd * Mlglu.mdd =
  let single = true in
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI, blameO, d0, d1 *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  let d0  = Symprog.get_delta0 sp in
  let d1  = Symprog.get_delta1 sp in
  (* literals for the above variables *)
  let bli_true = Mlglu.mdd_literal mgr bli [1] in
  let blo_true = Mlglu.mdd_literal mgr blo [1] in
  let d0true  = Mlglu.mdd_literal mgr d0  [1] in
  let d1true  = Mlglu.mdd_literal mgr d1  [1] in

  (* variables we deal with *)
  let vars = Symmod.get_vars sm in
  let cvars = Symmod.get_cvars sm in
  let not_cvars = VS.diff vars cvars in
  let vars' = Symprog.prime_vars sp vars in
  (* Warning: I'm assuming that the sets vars and vars' are converted
     into lists in the same order. Is this safe? Marco *)
  let var_list = Vset.to_list vars in
  let var_list' = Vset.to_list vars' in
  let var_list'' = Symprog.get_extra_vars sp var_list in

  let copy_x_x'  = Symutil.unchngd sp vars in
  let copy_x_x'' = Mlglu.mdd_substitute_two_lists mgr 
    copy_x_x' var_list' var_list'' in
  
  (* some popular predicates *)
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let iinv' = Mlglu.mdd_substitute_two_lists mgr 
    iinv var_list var_list' in
  let oinv' = Mlglu.mdd_substitute_two_lists mgr 
     oinv var_list var_list' in 
  (* the mdd representing a time step *)
  let delta1 = Symmod.get_delta1 sm in
  (* add the constraint that non-clock variables keep their value
     during a time step *)
  let delta1 = Symutil.and_unchngd sp delta1 not_cvars in

  (* build a list of all input transitions AND-ed with iinv' *)
  let itrans = ref([]) in
  let do_one_rule r : unit =
    if (Symmod.get_rule_act r != Symprog.env_act) then begin
      let m = Ops.get_transition_rel_noinv sp sm r in
      let minv = Mlglu.mdd_and m iinv' 1 1 in
      itrans := List.append !itrans [minv];
    end
  in
  Symmod.iter_irules sm do_one_rule;
  
  (* build a list of all output/local transitions AND-ed with  oinv' *)
  let otrans = ref([]) in
  let do_one_rule r : unit =
    let m = Ops.get_transition_rel_noinv sp sm r in
    let minv = Mlglu.mdd_and m oinv' 1 1 in
    otrans := List.append !otrans [minv];
  in
  Symmod.iter_lrules sm do_one_rule;
  Symmod.iter_orules sm do_one_rule;  

  (* For readability, blame variables are named as if
     Input played first. *)
  let (trans_first, trans_second, inv_first', inv_second, 
  bl_first, bl_first_true, bl_second_true) =
    if input_first then
      (!itrans, !otrans, iinv', oinv, bli, bli_true, blo_true)
    else
      (!otrans, !itrans, oinv', iinv, blo, blo_true, bli_true)
  in
  let tau1= ref([]) in
  let tau2 = ref([]) in
  
  let term_delta0 =  Mlglu.mdd_and d0true d1true 1 0 in
  print_mdd "1d0" mgr term_delta0; 
  if single then begin
    let term_delta0 = Mlglu.mdd_and term_delta0 copy_x_x' 1 1 in
    let term_delta0 = Mlglu.mdd_and term_delta0 copy_x_x'' 1 1 in
     tau1 := List.append !tau1 [(term_delta0, false, false)];
    end
  else   
  tau1 := List.append !tau1 [(term_delta0 , true, true)];  

  let delta1_and_inv = Mlglu.mdd_and delta1 inv_first' 1 1 in
  let term_delta1 = Mlglu.mdd_smooth mgr delta1_and_inv vars' in
  let term_delta1 = Mlglu.mdd_and term_delta1 d1true 1 1 in
  let term_delta1 = Mlglu.mdd_and term_delta1 d0true 1 0 in
  if single then begin
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
     tau1 := List.append !tau1 [(term_delta1, false, false)];
    end
  else   
   tau1 := List.append !tau1 [(term_delta1, true, true)];

  let term_action_first = Mlglu.mdd_and d1true d0true 0 0 in
  
  let do_one_action_first  m : unit =
    let m' =  Mlglu.mdd_substitute_two_lists mgr m var_list' var_list'' in
    let m' = Mlglu.mdd_and m' term_action_first 1 1 in 
    print_mdd "act1" mgr m'; 
    if single then begin
      let m' = Mlglu.mdd_and m' copy_x_x' 1 1 in
      tau1 := List.append !tau1 [(m', false, false)];
    end
  else   
    tau1 := List.append !tau1 [(m', true, false)];   
  in
  List.iter do_one_action_first trans_first;

  
  let var_var' = List.append var_list  var_list' in
  let var'_var = List.append var_list' var_list  in
  
  let d0_or_d1 = Mlglu.mdd_or d0true d1true 1 1 in
  let temp1 = Mlglu.mdd_xnor bl_second_true d0_or_d1 in
  let term_delta0 = Mlglu.mdd_neq_s mgr bl_first d1 in
  let term_delta0 = Mlglu.mdd_and term_delta0 temp1 1 1 in
  print_mdd "2d0" mgr term_delta0;
  if single then begin
    let term_delta0 = Mlglu.mdd_and term_delta0 copy_x_x'' 1 1 in
     tau2 := List.append !tau2 [(term_delta0, false, false)];
    end
  else   
  tau2 := List.append !tau2 [(term_delta0, false, true)]; 
  
  let inverted_delta1 = 
    Mlglu.mdd_substitute_two_lists mgr delta1 var_var' var'_var in
  let inverted_delta1_and_inv = 
    Mlglu.mdd_and inverted_delta1 inv_second 1 1 in
  let temp = Mlglu.mdd_smooth mgr inverted_delta1_and_inv vars in
  let term_delta1 = Mlglu.mdd_and bl_first_true bl_second_true 1 0 in
  let term_delta1 = Mlglu.mdd_and term_delta1 d1true 1 0 in
  let term_delta1 = Mlglu.mdd_and term_delta1 temp 1 1 in
  print_mdd "2d1" mgr term_delta1;
  if single then begin
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
     tau2 := List.append !tau2 [(term_delta1, false, false)];
    end
  else         
  tau2 := List.append !tau2 [(term_delta1, false, true)];

  let term_delta1_bis = Mlglu.mdd_and bl_first_true bl_second_true 0 0 in
  let term_delta1_bis = Mlglu.mdd_and term_delta1_bis d1true 1 1 in
  let term_delta1_bis = Mlglu.mdd_and 
    term_delta1_bis inverted_delta1_and_inv 1 1 in
  print_mdd "2d1bis" mgr term_delta1_bis;
  tau2 := List.append !tau2 [(term_delta1_bis, false, false)];
 
  let term_action = Mlglu.mdd_and bl_first_true bl_second_true 0 1 in
  let do_one_action_second m =
    let inverted_m = Mlglu.mdd_substitute_two_lists mgr m
      var_var' var'_var in
    let inverted_m = Mlglu.mdd_and inverted_m term_action 1 1 in
    print_mdd "act2" mgr inverted_m;
    tau2 := List.append !tau2 [(inverted_m,false,false)]
  in
  List.iter do_one_action_second trans_second; 
  !tau1, !tau2, copy_x_x', copy_x_x'';;

(** Performs the pre-computations that are useful to Cpre.
  Argument [input_first] fixes which player moves first.
  In particular, computes the transition relations 
  for both players.
 *)
let cpre_init sp sm (input_first: bool) =
  let mgr = Symprog.get_mgr sp in
  let (tau_first, tau_second, prime, dprime) = turn_transitions sp sm input_first in

  (* variables we deal with *)
  let vars = Symmod.get_vars sm in
  let vars' = Symprog.prime_vars sp vars in
  let var_list = Vset.to_list vars in
  let var_list' = Vset.to_list vars' in 
  let var_list'' = Symprog.get_extra_vars sp var_list in 

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  let d0  = Symprog.get_delta0 sp in
  let d1  = Symprog.get_delta1 sp in

  let var_first  = var_list' @ var_list'' @ [d0; d1] in
  let var_second = var_list @ [bli; blo] in
  (mgr, tau_first, tau_second, var_first, var_second, prime, dprime)

(** Computes the controllable predecessor operator for either player.

  The first argument provides the required predicates.
  If the first argument is the result of cpreI_init, we obtain CpreI;
  If the first argument is the result of cpreO_init, we obtain CpreO.
  IMPORTANT: tau_first and tau_second are lists of mdds. 
 *)
let cpre stuff (first_is_controlling: bool) m : Mlglu.mdd =

  let (mgr, tau_first, tau_second, var_first, var_second, prime, dprime) = stuff in
  let one =  Mlglu.mdd_one mgr in
  let zero = Mlglu.mdd_zero mgr in 
  let inv_trans_or old vlist a tr : Mlglu.mdd =
      let (t,p,d) = tr in
      let tmp = Mlglu.mdd_or t old 0 1 in
      let tmp1 = if p then (Mlglu.mdd_or tmp prime 1 0) else tmp in
      let tmp2 = if d then (Mlglu.mdd_or tmp1 dprime 1 0) else tmp1 in
      let tmp3 = Mlglu.mdd_consensus_list mgr tmp2 vlist in
      Mlglu.mdd_and a tmp3 1 1  
    in 
   let trans_and old vlist b tr: Mlglu.mdd =
      let (t,p,d) = tr in
      let tmp = Mlglu.mdd_and old t 1 1 in
      let tmp1 = if p then (Mlglu.mdd_and tmp prime 1 1) else tmp in
      let tmp2 = if d then (Mlglu.mdd_and tmp1 dprime 1 1) else tmp1 in
      let tmp3 = Mlglu.mdd_smooth_list mgr tmp2 vlist in
      Mlglu.mdd_or b tmp3 1 1    
   in
    if (first_is_controlling) then begin    
      let newterm = List.fold_left (inv_trans_or m var_second) one tau_second in   
       List.fold_left (trans_and newterm var_first) zero tau_first
    end
    else
     begin
       let newterm = List.fold_left (trans_and m var_second) zero tau_second in
       List.fold_left (inv_trans_or newterm var_first) one tau_first
    end
 (* will return may cpre and must cpre for a choosen player *)

(** given a cpre operator and 3 colors of states, returns the result of the
 iterative fixpoint computation *)

let compute_3color_winning mgr cpre color0 color1 color2 inv: Mlglu.mdd =
  let z = ref (Mlglu.mdd_one mgr) 
  and z_diff = ref (Mlglu.mdd_one mgr) in
 
   

  while not (Mlglu.mdd_is_zero !z_diff) do
    let z_term = cpre !z in
    let z_term = Mlglu.mdd_and z_term color0 1 1 in
    print_string "Z";
    flush stdout;
     let y = ref (Mlglu.mdd_zero mgr) 
  and y_diff = ref (Mlglu.mdd_one mgr) in

    while not (Mlglu.mdd_is_zero !y_diff) do
      let y_term = cpre !y in
      let y_term = Mlglu.mdd_and y_term color1 1 1 in
      print_string "Y";
      flush stdout;
     
     let x = ref (Mlglu.mdd_one mgr)
     and x_diff = ref (Mlglu.mdd_one mgr) in
     
      while not (Mlglu.mdd_is_zero !x_diff) do
	let x_term = cpre !x in
	let x_term = Mlglu.mdd_and x_term color2 1 1 in
        print_string "X";
        flush stdout;
	let all_terms = Mlglu.mdd_or x_term y_term 1 1 in
	let all_terms = Mlglu.mdd_or all_terms z_term 1 1 in
	x_diff := Mlglu.mdd_and !x all_terms 1 0;
	x := all_terms;
      done;
      y_diff := Mlglu.mdd_and !x !y 1 0;
      y := !x;
    done;
    z_diff := Mlglu.mdd_and !z !y 1 0;
    z := !y;
  done;
  (* so far, live states could contain states that violate the
     player's invariant. In particuar, these would be states from which
     you can go back to the invariant in one step.
     Thus, we conjoin with the appropriate invariant. *)
  Mlglu.mdd_and !z inv 1 1;;

(** If [input] is true, computes the set of states 
  where Input has a strategy to let time diverge 
  or blame the adversary (the set of I-live states), i.e.

  infinitely often tick or eventually forever blame_O

  If [input] is false, computes the set of O-live states, i.e. 

  infinitely often tick or eventually forever not blame_O
  (equivalently, 
  infinitely often tick or eventually forever blame_I and not blame_O)

  Uses the Emerson-Jutla algorithm based on a triple fixpoint. *)

let live_cpre input sp ?(verbose : bool = true) sm =
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in
  
  (* We choose the semantics of Timed Interfaces,
     where Output moves first both in the I-liveness game
     and in the O-liveness game *)
  let input_moves_first = false in
  let first_player_is_controlling = not input in
  
  (* the third argument of [cpre_init] controls 
     who moves first (not whose Cpre it is) *)
  let initstuff = cpre_init sp sm input_moves_first in
  let (stuff, inv, color0, color1, color2) =
    if input then
      ( initstuff,
      Symmod.get_iinv sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      Mlglu.mdd_and blitrue blotrue 1 0,
      blotrue )
    else
      ( initstuff,
      Symmod.get_oinv sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      blotrue,
      Mlglu.mdd_and blitrue blotrue 1 0 )
  in
  let cpre1 =  cpre stuff first_player_is_controlling in
  let iinv =  compute_3color_winning mgr cpre1 color0 color1 color2 inv in
  (*let (maycpre, mustcpre) = cpre stuff first_player_is_controlling in
  let winm = compute_3color_winning mgr maycpre color0 color1 color2 inv in
  let winM = compute_3color_winning mgr mustcpre color0 color1 color2 inv in*)
 (* do something smarter to obtain the refinement*)
 (* print_mdd "iinv" mgr iinv;*)
  iinv;;

(* exported functions *)
let i_live = live_cpre true
let o_live = live_cpre false  

let bothinv sp ?(verbose : bool = true) sm =
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in
  
  (* We choose the semantics of Timed Interfaces,
     where Output moves first both in the I-liveness game
     and in the O-liveness game *)
  (*let input_moves_first = false in
  let first_player_is_controlling = not input in*)
  
  (* the third argument of [cpre_init] controls 
     who moves first (not whose Cpre it is) *)
  let stuff = cpre_init sp sm false in
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let bothfalse = Mlglu.mdd_and blitrue blotrue 0 0 in
  let truefalse =  Mlglu.mdd_and blitrue blotrue 1 0 in
  let blameout = blotrue in
  let cpreI =  cpre stuff false in
  let cpreO =  cpre stuff true in 
  let siinv = compute_3color_winning mgr cpreI bothfalse truefalse blameout iinv in
  let soinv = compute_3color_winning mgr cpreO bothfalse blameout truefalse oinv in
  siinv, soinv;;
