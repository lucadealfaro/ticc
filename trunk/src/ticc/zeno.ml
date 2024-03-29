(** This module contains the algorithms which deal with
  non-zenoness. *)

open Vset;;

let print_time msg = ()
  (*  print_string msg;
    let time = Sys.time () in
    Printf.printf " %f\n" time;
    flush stdout *)
;;

(** Returns the set of states which have a measure which
    is separated from 0.
    In other words, returns all states that have a measure value m > 0
    such that there is a value n < m such that
    no state has value n. *)
let separated sp measure vars rho =
  let mgr = Mlglu.mdd_get_manager measure in
  let not_m = Mlglu.mdd_not measure in
  let always_not_m = Mlglu.mdd_consensus_list mgr not_m vars in
  (* the mdd assigning to rho the value of the minimum gap *)
  let min_gap = Mlglu.mdd_min always_not_m rho in

  (* let tmp = Mlglu.mdd_get_support mgr min_gap in *)
  (* Symutil.print_varset_rough sp tmp; *)

  (* the actual integral value of the minimum gap *)
  let x = Mlglu.mdd_get_unique_value mgr min_gap rho in
  let rho_gt_x = Mlglu.mdd_gt_c mgr rho x in
  let result = Mlglu.mdd_and measure rho_gt_x 1 1 in
  let result = Mlglu.mdd_smooth_list mgr result [rho] in
  (* Printf.printf "x = %d \n" x; *)
  result
;;


(** If [input_first] is true, returns the transition relations 
  of the turn-based game where input plays before output.
  Otherwise, returns the transition relations 
  of the turn-based game where output plays before input.
 *)
let turn_transitions sp sm (input_first: bool) =
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

  let copy_x_x'  = Symutil.unchngd sp vars in
  let copy_x_x'' = Mlglu.mdd_substitute_two_lists mgr 
    copy_x_x' var_list' var_list'' in

  (* build the disjunction of all input transitions *)
  let itrans = ref (Mlglu.mdd_zero mgr) in
  let do_one_rule r : unit =
    (* The envorinment rule leads to a weird timed semantics.
       Even if the module has no stateless variables,
       there is still an environment move stating that all
       variables keep their value.
       Thus, Input can get the game stuck in a state by always
       playing the environment move.
       
       Since nondeterminism is adversarial (no fairness), 
       Output cannot get anything done! *)
    if (Symmod.get_rule_act r != Symprog.env_act) then begin
      let m = Ops.get_transition_rel_noinv sp sm r in
      itrans := Mlglu.mdd_or !itrans m 1 1;
    end
  in
  Symmod.iter_irules sm do_one_rule;
  (* conjoin with input invariant *)
  itrans := Mlglu.mdd_and !itrans iinv' 1 1;

  (* build the disjunction of all output and local transitions *)
  let otrans = ref (Mlglu.mdd_zero mgr) in
  let do_one_rule r : unit =
    let m = Ops.get_transition_rel_noinv sp sm r in
    otrans := Mlglu.mdd_or !otrans m 1 1;
  in
  Symmod.iter_lrules sm do_one_rule;
  Symmod.iter_orules sm do_one_rule;
  (* conjoin with output invariant *)
  otrans := Mlglu.mdd_and !otrans oinv' 1 1;

  (* For readability, blame variables are named as if
     Input played first. *)
  let (trans_first, trans_second, inv_first', inv_second, 
  bl_first, bl_first_true, bl_second_true) =
    if input_first then
      (!itrans, !otrans, iinv', oinv, bli, bli_true, blo_true)
    else
      (!otrans, !itrans, oinv', iinv, blo, blo_true, bli_true)
  in
  
  (* the transition relation of the first player to move *)
  let tau_first =
    
    let term_delta0 = Mlglu.mdd_and copy_x_x''  d0true 1 1 in
    let term_delta0 = Mlglu.mdd_and term_delta0 d1true 1 0 in

    let delta1_and_inv = Mlglu.mdd_and delta1 inv_first' 1 1 in
    let term_delta1 = Mlglu.mdd_smooth mgr delta1_and_inv vars' in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 d1true 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 d0true 1 0 in

    (* rename variables: X' -> X'' *)
    let trans_first = Mlglu.mdd_substitute_two_lists mgr trans_first 
      var_list' var_list'' in
    let term_action = Mlglu.mdd_and trans_first d0true 1 0 in
    let term_action = Mlglu.mdd_and term_action d1true 1 0 in

    (* debug term_action "term_action";
       debug term_delta0 "term_delta0";
       debug term_delta1 "term_delta1"; *)
    
    let res = Mlglu.mdd_or term_delta0 term_delta1 1 1 in
    let res = Mlglu.mdd_or res term_action 1 1 in
    Mlglu.mdd_and res copy_x_x' 1 1
  in

  (* the transition relation of the second player to move.
     For readability, blame variables are named as if
     Input played first. *)
  let tau_second =
    (* rename variables: (X,X') -> (X',X) *)
    let var_var' = List.append var_list  var_list' in
    let var'_var = List.append var_list' var_list  in
    let trans_second = Mlglu.mdd_substitute_two_lists mgr trans_second
      var_var' var'_var in

    let d0_or_d1 = Mlglu.mdd_or d0true d1true 1 1 in
    let temp = Mlglu.mdd_xnor bl_second_true d0_or_d1 in
    let term_delta0 = Mlglu.mdd_neq_s mgr bl_first d1 in
    let term_delta0 = Mlglu.mdd_and term_delta0 temp 1 1 in
    let term_delta0 = Mlglu.mdd_and term_delta0 copy_x_x'' 1 1 in

    let inverted_delta1 = 
      Mlglu.mdd_substitute_two_lists mgr delta1 var_var' var'_var in
    let inverted_delta1_and_inv = 
      Mlglu.mdd_and inverted_delta1 inv_second 1 1 in
    let temp = Mlglu.mdd_smooth mgr inverted_delta1_and_inv vars in
    let term_delta1 = Mlglu.mdd_and bl_first_true bl_second_true 1 0 in
    let term_delta1 = Mlglu.mdd_and term_delta1 d1true 1 0 in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 temp 1 1 in

    let term_delta1_bis = Mlglu.mdd_and bl_first_true bl_second_true 0 0 in
    let term_delta1_bis = Mlglu.mdd_and term_delta1_bis d1true 1 1 in
    let term_delta1_bis = Mlglu.mdd_and 
      term_delta1_bis inverted_delta1_and_inv 1 1 in

    let term_action = Mlglu.mdd_and bl_first_true bl_second_true 0 1 in
    let term_action = Mlglu.mdd_and term_action trans_second 1 1 in

    let res = Mlglu.mdd_or term_delta0 term_delta1 1 1 in
    let res = Mlglu.mdd_or res term_delta1_bis 1 1 in
    Mlglu.mdd_or res term_action 1 1
  in

  (** DEBUG *)
  (* Printf.printf " tau_first: \n"; flush stdout;
     let temp = Mlglu.mdd_and tau_first d1true 1 1 in
     let smooth_list = List.append var_list' var_list'' in
     let temp = Mlglu.mdd_smooth_list mgr temp smooth_list in
     Mlglu.mdd_print mgr temp;
     Printf.printf " end tau_first \n"; flush stdout; *)

  (** DEBUG *)
  (* Printf.printf " tau_first: \n"; flush stdout;
     Mlglu.mdd_print mgr tau_first;
     Printf.printf " end tau \n"; flush stdout; 
     
     Printf.printf " tau_second: \n"; flush stdout;
     Mlglu.mdd_print mgr tau_second;
     Printf.printf " end tau \n"; flush stdout; *)
  
  (tau_first, tau_second)
;;


(** Computes the set of states where Input has a strategy
    to let time diverge or blame the adversary.
    Uses Jurdzinski's progress measure algorithm.
    Refer to the techrep 06-timed-ticc for details. 
 *)
let i_live_pmeasure sp ?(gap: bool = true) sm =

  let mgr = Symprog.get_mgr sp in
  (* the variable representing the progress measure *)
  let rho = Symprog.get_measure_var sp sm in
  (* maximum value of rho (progress measure) *)
  let max_val = (Mlglu.mdd_get_var_range mgr rho) -1 in
  (* predicate rho = rho_max *)
  let rho_max = Mlglu.mdd_literal mgr rho [max_val] in
  (* predicate rho = 0 *)
  let rho_zero = Mlglu.mdd_literal mgr rho [0] in

  Printf.printf "max value of rho=%d \n" max_val; flush stdout;
  print_time "start:";

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  let d0  = Symprog.get_delta0 sp in
  let d1  = Symprog.get_delta1 sp in
  (* literals for the above variables *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in

  (* slices of the parity games *)
  let color0 = Mlglu.mdd_and blitrue blotrue 0 0 in
  let color1 = Mlglu.mdd_and blitrue blotrue 1 0 in
  let color2 = blotrue in
  (* variables we deal with *)
  let vars  = Symmod.get_vars sm in
  let vars' = Symprog.prime_vars sp vars in
  (* Warning: I'm assuming that the sets vars and vars' are converted
     into lists in the same order. Is this safe? Marco *)
  let var_list = Vset.to_list vars in
  let var_list' = Vset.to_list vars' in
  let var_list'' = Symprog.get_extra_vars sp var_list in
  (* we adopt the semantics where output always moves first *)
  let (tau_first, tau_second) = turn_transitions sp sm false in

  (* list of variables to be smoothed *)
  let smoothy_second = bli::blo::var_list in
  let smoothy_first = d0::(d1::(var_list'@var_list'')) in

  (* lift operator for the second player to move.
     The second player moves from "virtual states" that are
     assumed to have color 2.
     Therefore, this lift operation is straightforward.

     Given the current measure for regular states 
     and the old measure for virtual states,
     it returns the new measure for virtual states. 

     This is the _first_ lift to be performed in every algorithm iteration.

     Types: curr_m[vars, blI, blO, rho]
            tau_second[vars', vars'', d0, d1; X, blI, blO] 
            old_m[vars', vars'', d0, d1, rho] *)
  let lift_second (curr_m: Mlglu.mdd) (old_m: Mlglu.mdd) : Mlglu.mdd =
    let res = Mlglu.mdd_and tau_second curr_m 1 1 in

    (* the sequence a -> b takes a "long" time *)
    print_time "a:";
    let res = Mlglu.mdd_smooth_list mgr res smoothy_second in
    print_time "b:";

    (** IMPORTANT: we are assuming the second player 
      is the controlling player (minimizer) *)
    let result = Mlglu.mdd_min res rho in

    (* optionally apply the gap optimization *)
    if gap then begin
      (* take maximum with respect to old measure for virtual states *)
      let temp = Mlglu.mdd_or result old_m 1 1 in
      Mlglu.mdd_max temp rho 
    end else
      res
  in

  (* lift operator for the first player to move.

     Given the current measure for virtual states 
     and the old measure for regular states,
     it returns the new measure for regular states. 

     This is the _second_ lift to be performed in every algorithm iteration.

     Types: old_m[vars, blI, blO, rho]
            tau_first[vars; vars', vars'', d0, d1] 
            curr_m[vars', vars'', d0, d1, rho]
     output: [vars, blI, blO, rho] *)
  let lift_first (old_m: Mlglu.mdd) (curr_m: Mlglu.mdd) : Mlglu.mdd =

    let measure' = curr_m in
      (* was: Mlglu.mdd_substitute_two_lists mgr curr_m var_list
	 var_list' in *)

    (* the sequence c -> d takes a "long" time *)
    print_time "c:";
    let res = Mlglu.mdd_and tau_first measure' 1 1 in
    let res = Mlglu.mdd_smooth_list mgr res smoothy_first in
    print_time "d:";

    (** IMPORTANT: we are assuming the first player 
      is the adversary (maximizer) *)
    let minsucc = Mlglu.mdd_max res rho in
    let incr_minsucc = Mlglu.mdd_incr minsucc rho in

    (* let temp = Mlglu.mdd_and minsucc incr_rho 1 1 in
       let temp = Mlglu.mdd_smooth_list mgr temp [rho] in
       let temp = Mlglu.mdd_substitute_two_lists mgr temp [rho'] [rho] in
    (* add the term for rho = max_val *)
       let keep_value = Mlglu.mdd_and minsucc rho_max 1 1 in
       Mlglu.mdd_or temp keep_value 1 1 
       in *)

    let term0 = Mlglu.mdd_and rho_max minsucc 1 1 in
    let temp  = Mlglu.mdd_lt_c mgr rho max_val    in
    let temp  = Mlglu.mdd_and temp    minsucc 1 1 in
    let temp  = Mlglu.mdd_smooth_list mgr temp [rho] in
    let temp  = Mlglu.mdd_and temp  rho_zero 1 1 in
    let term0 = Mlglu.mdd_or  term0 temp     1 1 in
    let term0 = Mlglu.mdd_and term0 color0   1 1 in

    let term1 = Mlglu.mdd_and color1 incr_minsucc 1 1 in

    let term2 = Mlglu.mdd_and color2 minsucc 1 1 in

    let res = Mlglu.mdd_or term0 term1 1 1 in
    let res = Mlglu.mdd_or res term2 1 1 in

    (* optionally apply the gap optimization *)
    if gap then begin
      (* take maximum with respect to old regular measure *)
      let temp = Mlglu.mdd_or res old_m 1 1 in
      Mlglu.mdd_max temp rho
    end else
      res
  in

  let mI     = ref rho_zero in
  let new_mI = ref rho_zero in
  let mO     = ref rho_zero in
  let diff   = ref (Mlglu.mdd_one mgr) in
  (* all variables in the progress measure *)
  let measure_vars = bli :: (blo :: var_list) in

  while not (Mlglu.mdd_is_zero !diff) do

    print_time "cycle starts:";

    (* update output measure *)
    mO     := lift_second !mI !mO;
    (* update input measure *)
    new_mI := lift_first !mI !mO;
    (* display a progress indicator *)
    print_string ".";
    flush stdout;

    if gap then begin
      (* 1. find states that are separated from "rho = 0" *)
      let losers = separated sp !new_mI measure_vars rho in
      (* 2. for those states, set "rho = top" *)
      let losers_to_top = Mlglu.mdd_and losers rho_max 1 1 in
      (* 3. find states that are not separated from "rho = 0" *)
      let not_losers = Mlglu.mdd_and !new_mI losers 1 0 in
      (* 4. update measure for Input *)
      new_mI := Mlglu.mdd_or not_losers losers_to_top 1 1
    end;
    (* let debug_mdd = Mlglu.mdd_not new_measure in
       let debug_mdd = Mlglu.mdd_consensus_list mgr debug_mdd [rho] in
       if not (Mlglu.mdd_is_zero debug_mdd) then begin
       debug_always debug_mdd "states with no measure";
       raise Not_found;
       end; *)

    diff := Mlglu.mdd_and !new_mI !mI 1 0;
    mI := !new_mI;

    print_time "cycle ends:";

  done;
  print_string "\n";
  (* states with maximum value of rho are losing *)
  let losers = Mlglu.mdd_and !mI rho_max 1 1 in
  let losers = Mlglu.mdd_smooth_list mgr losers [rho] in
  let winners = Mlglu.mdd_not losers in
  (* so far, winning (I-live) states could violate
     the Input invariant. We fix this here. *)
  let iinv = Symmod.get_iinv sm in
  Mlglu.mdd_and winners iinv 1 1

    
(** Performs the pre-computations that are useful to Cpre.
  Argument [input_first] fixes which player moves first.
  In particular, computes the transition relations 
  for both players.
 *)
let cpre_init sp sm (input_first: bool) =
  let mgr = Symprog.get_mgr sp in
  let (tau_first, tau_second) = turn_transitions sp sm input_first in

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
  (mgr, tau_first, tau_second, var_first, var_second)


(** Computes the controllable predecessor operator for either player.

  The first argument provides the required predicates.
  If the first argument is the result of cpreI_init, we obtain CpreI;
  If the first argument is the result of cpreO_init, we obtain CpreO.
 *)
let cpre stuff (first_is_controlling: bool) m =

  let (mgr, tau_first, tau_second, var_first, var_second) = stuff in
  if (first_is_controlling) then
    let implication = Mlglu.mdd_or tau_second m 0 1 in
    let term = Mlglu.mdd_consensus_list mgr implication var_second in
    let term = Mlglu.mdd_and term tau_first 1 1 in
    Mlglu.mdd_smooth_list mgr term var_first
  else
    let tau_second_and_m = Mlglu.mdd_and tau_second m 1 1 in
    let term = Mlglu.mdd_smooth_list mgr tau_second_and_m var_second in
    let term = Mlglu.mdd_or tau_first term 0 1 in
    Mlglu.mdd_consensus_list mgr term var_first
      

(** If [input] is true, computes the set of states 
  where Input has a strategy to let time diverge 
  or blame the adversary (the set of I-live states), i.e.

  infinitely often tick or eventually forever blame_O

  If [input] is false, computes the set of O-live states, i.e. 

  infinitely often tick or eventually forever not blame_O
  (equivalently, 
  infinitely often tick or eventually forever blame_I and not blame_O)

  Uses the Emerson-Jutla algorithm based on a triple fixpoint. *)
let live_cpre input sp ?(verbose : bool = false) sm =
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
  let (stuff, inv, color0, color1, color2) =
    if input then
      ( cpre_init sp sm input_moves_first,
      Symmod.get_iinv sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      Mlglu.mdd_and blitrue blotrue 1 0,
      blotrue )
    else
      ( cpre_init sp sm input_moves_first,
      Symmod.get_oinv sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      blotrue,
      Mlglu.mdd_and blitrue blotrue 1 0 )
  in

  let my_cpre = cpre stuff first_player_is_controlling in

  let z = ref (Mlglu.mdd_one mgr) 
  and z_diff = ref (Mlglu.mdd_one mgr) in

  print_time "start main loop:";
  
  while not (Mlglu.mdd_is_zero !z_diff) do
    let z_term = my_cpre !z in
    let z_term = Mlglu.mdd_and z_term color0 1 1 in

    (* display a progress indicator *)
    if verbose then begin
      print_string "Z";
      flush stdout;
    end else begin
      print_string ".";
      flush stdout;
    end;
    
    let y = ref (Mlglu.mdd_zero mgr) 
    and y_diff = ref (Mlglu.mdd_one mgr) in

    while not (Mlglu.mdd_is_zero !y_diff) do
      let y_term = my_cpre !y in
      let y_term = Mlglu.mdd_and y_term color1 1 1 in

      if verbose then begin
	print_string "y";
	flush stdout;
      end;

      let x = ref (Mlglu.mdd_one mgr)
      and x_diff = ref (Mlglu.mdd_one mgr) in
     
      while not (Mlglu.mdd_is_zero !x_diff) do
	let x_term = my_cpre !x in
	let x_term = Mlglu.mdd_and x_term color2 1 1 in

	if verbose then begin
	  print_string "x";
	  flush stdout;
	end;

	let all_terms = Mlglu.mdd_or x_term y_term 1 1 in
	let all_terms = Mlglu.mdd_or all_terms z_term 1 1 in
	x_diff := Mlglu.mdd_and !x all_terms 1 0;
	x := all_terms;

      (** DEBUG *)
      (* Printf.printf " x: \n"; flush stdout;
	 Mlglu.mdd_print mgr !x;
	 Printf.printf " end x \n"; flush stdout; *)
      done;

      y_diff := Mlglu.mdd_and !x !y 1 0;
      y := !x;
    done;
    
    z_diff := Mlglu.mdd_and !z !y 1 0;
    z := !y;
  done;

  print_time "end:";
  (* so far, live states could contain states that violate the
     player's invariant. In particuar, these would be states from which
     you can go back to the invariant in one step.
     Thus, we conjoin with the appropriate invariant. *)
  Mlglu.mdd_and !z inv 1 1


(** Exported functions *)
let i_live = live_cpre true
let o_live = live_cpre false 
let i_live_alt = i_live_pmeasure ~gap:true


(** Winning set of the composition game.

  Let good be the set of locally compatible states.
  The composition game has goal:

     (Always good) and (time divergence or blame Output)

  We first solve the safety goal (Always good).
  Then, we solve the liveness goal while forcing Input to always
  stay in the safety winning set.
 *)
let win_composition (sp: Symprog.t) (sm: Symmod.t) (good: Symmod.stateset_t) :
    Symmod.stateset_t =

  (* store current Input invariant *)
  let iinv = Symmod.get_iinv sm in
  (* compute winning set of the safety part of the goal *)
  let win_safe = Ops.win_i_safe sp sm good in
  (* force Input to stay in win_safe *)
  Symmod.set_iinv sm win_safe;
  (* play the liveness game *)
  let win_live = i_live sp sm in
  (* restore Input invariant: we are not supposed to modify the module
   *)
  Symmod.set_iinv sm iinv;
  Mlglu.mdd_and win_safe win_live 1 1
