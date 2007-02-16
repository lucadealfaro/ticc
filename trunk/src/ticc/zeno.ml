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


(** Returns the transition relations of the turn-based game where
    input plays before output.
 *)
let io_transitions sp sm =
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in

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

  (* the input transition relation *)
  let tauI =
    (* build the disjunction of all input transitions *)
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_irules sm do_one_rule;
    (* conjoin with input invariant *)
    trans := Mlglu.mdd_and !trans iinv' 1 1;
    (* rename variables: X' -> X'' *)
    trans := Mlglu.mdd_substitute_two_lists mgr !trans var_list' var_list'';

    let term_action = Mlglu.mdd_and blitrue !trans 1 1 in
    let term_delta0 = Mlglu.mdd_and blitrue copy_x_x'' 1 1 in
    let delta1_and_iinv = Mlglu.mdd_and delta1 iinv' 1 1 in
    let term_delta1 = Mlglu.mdd_smooth mgr delta1_and_iinv vars' in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 blitrue 1 0 in

    (* debug term_action "term_action";
       debug term_delta0 "term_delta0";
       debug term_delta1 "term_delta1"; *)
    
    let res = Mlglu.mdd_or term_delta0 term_delta1 1 1 in
    let res = Mlglu.mdd_or res term_action 1 1 in
    Mlglu.mdd_and res copy_x_x' 1 1
  in

  (* the output transition relation *)
  let tauO =
    (* build the disjunction of all output and local transitions *)
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_lrules sm do_one_rule;
    Symmod.iter_orules sm do_one_rule;
    (* conjoin with output invariant *)
    trans := Mlglu.mdd_and !trans oinv' 1 1;
    (* rename variables: (X,X') -> (X',X) *)
    let var_var' = List.append var_list  var_list' in
    let var'_var = List.append var_list' var_list  in
    trans := Mlglu.mdd_substitute_two_lists mgr !trans var_var' var'_var;

    let term1 = Mlglu.mdd_or  copy_x_x'' !trans  1 1 in
    let term1 = Mlglu.mdd_and term1   blotrue    1 1 in
    let inverted_delta1 = 
      Mlglu.mdd_substitute_two_lists mgr delta1 var_var' var'_var in
    let inverted_delta1_and_oinv = 
      Mlglu.mdd_and inverted_delta1 oinv 1 1 in
    let term2 = Mlglu.mdd_smooth mgr inverted_delta1_and_oinv vars in
    let term2 = Mlglu.mdd_and term2 blitrue    1 1 in
    let term2 = Mlglu.mdd_and term2 blotrue    1 0 in
    let term2 = Mlglu.mdd_and term2 copy_x_x'' 1 1 in
    let term3 = Mlglu.mdd_and blitrue blotrue    0 0 in
    let term3 = Mlglu.mdd_and term3 inverted_delta1_and_oinv 1 1 in
    let res   = Mlglu.mdd_or  term1 term2 1 1 in
    Mlglu.mdd_or res term3 1 1
  in
  (tauI, tauO)
;;


(** Returns the transition relations of the turn-based game where
    output plays before input.
 *)
let oi_transitions sp sm =
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in

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

  (* the input transition relation *)
  let tauI =
    (* build the disjunction of all input transitions *)
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_irules sm do_one_rule;
    (* conjoin with input invariant *)
    trans := Mlglu.mdd_and !trans iinv' 1 1;
    (* rename variables: (X,X') -> (X',X) *)
    let var_var' = List.append var_list  var_list' in
    let var'_var = List.append var_list' var_list  in
    trans := Mlglu.mdd_substitute_two_lists mgr !trans var_var' var'_var;

    let term1 = Mlglu.mdd_or  copy_x_x'' !trans  1 1 in
    let term1 = Mlglu.mdd_and term1      blitrue 1 1 in
    let inverted_delta1 = 
      Mlglu.mdd_substitute_two_lists mgr delta1 var_var' var'_var in
    let inverted_delta1_and_iinv = 
      Mlglu.mdd_and inverted_delta1 iinv 1 1 in
    let term2 = Mlglu.mdd_smooth mgr inverted_delta1_and_iinv vars in
    let term2 = Mlglu.mdd_and term2 blotrue    1 1 in
    let term2 = Mlglu.mdd_and term2 blitrue    1 0 in
    let term2 = Mlglu.mdd_and term2 copy_x_x'' 1 1 in
    let term3 = Mlglu.mdd_and blitrue blotrue    0 0 in
    let term3 = Mlglu.mdd_and term3 inverted_delta1_and_iinv 1 1 in
    let res   = Mlglu.mdd_or  term1 term2 1 1 in
    Mlglu.mdd_or res term3 1 1
  in

  (* the output transition relation *)
  let tauO =
    (* build the disjunction of all output and local transitions *)
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_lrules sm do_one_rule;
    Symmod.iter_orules sm do_one_rule;
    (* conjoin with output invariant *)
    trans := Mlglu.mdd_and !trans oinv' 1 1;
    (* rename variables: X' -> X'' *)
    trans := Mlglu.mdd_substitute_two_lists mgr !trans var_list' var_list'';

    let term_action = Mlglu.mdd_and blotrue !trans 1 1 in
    let term_delta0 = Mlglu.mdd_and blotrue copy_x_x'' 1 1 in
    let delta1_and_oinv = Mlglu.mdd_and delta1 oinv' 1 1 in
    let term_delta1 = Mlglu.mdd_smooth mgr delta1_and_oinv vars' in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 blotrue 1 0 in

    let res = Mlglu.mdd_or term_delta0 term_delta1 1 1 in
    let res = Mlglu.mdd_or res term_action 1 1 in
    Mlglu.mdd_and res copy_x_x' 1 1
  in
  (tauO, tauI)
;;


(** Computes the set of states where Input has a strategy
    to let time diverge or blame the adversary.
    Uses Jurdzinski's progress measure algorithm.
    Refer to the techrep 06-timed-ticc for details. *)
let i_live_internal sp ?(gap: bool = true) sm =

  let mgr = Symprog.get_mgr sp in

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
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in

  (* slices of the parity games *)
  let color0 = Mlglu.mdd_and blitrue blotrue 0 0 in
  let color1 = Mlglu.mdd_and blitrue blotrue 1 0 in
  let color2 = blotrue in

  (* variables we deal with *)
  let vars = Symmod.get_vars sm in
  let vars' = Symprog.prime_vars sp vars in
  (* Warning: I'm assuming that the sets vars and vars' are converted
     into lists in the same order. Is this safe? Marco *)
  let var_list = Vset.to_list vars in
  let var_list' = Vset.to_list vars' in
  let var_list'' = Symprog.get_extra_vars sp var_list in

  (* some debugging functions *)
  (* let debug_jurdzinski = false in
     
     let debug mdd str =
     if debug_jurdzinski then begin
     Printf.printf "** %s=\n" str;
     flush stdout;
     Mlglu.mdd_print mgr mdd;
     flush stdout;
     end
     in
     let debug_always mdd str =
     Printf.printf "** %s=\n" str;
     flush stdout;
     Mlglu.mdd_print mgr mdd;
     flush stdout;
     in *)
  
  let (tauI, tauO) = io_transitions sp sm in
  (* debug tauI "tauI"; 
     debug tauO "tauO"; *)

  (* list of variables to be smoothed *)
  let smoothy_O = blo::var_list in
  let smoothy_I = bli::(var_list'@var_list'') in

  (* lift operator for Output.
     Given the current measures for input and output states,
     it returns the new measure for output states. *)
  let liftO (mI: Mlglu.mdd) (mO: Mlglu.mdd) : Mlglu.mdd =
    let res = Mlglu.mdd_and tauO mI 1 1 in

    (* the sequence a -> b takes a "long" time *)
    print_time "a:";
    let res = Mlglu.mdd_smooth_list mgr res smoothy_O in
    print_time "b:";

    (* optionally apply the gap optimization *)
    let new_res = if gap then begin
      (* take maximum with respect to old Output measure *)
      Mlglu.mdd_or res mO 1 1 
    end else
      res
    in
    Mlglu.mdd_max new_res rho
  in

  (* lift operator for Input.
     Given the current measures for input and output states,
     it returns the new measure for input states. *)
  let liftI (mI: Mlglu.mdd) (mO: Mlglu.mdd) : Mlglu.mdd =

    let measure' = Mlglu.mdd_substitute_two_lists mgr 
      mO var_list var_list' in

    (* the sequence c -> d takes a "long" time *)
    print_time "c:";
    let res = Mlglu.mdd_and tauI measure' 1 1 in
    let res = Mlglu.mdd_smooth_list mgr res smoothy_I in
    print_time "d:";

    let minsucc = Mlglu.mdd_min res rho in
    let incr_minsucc = Mlglu.mdd_incr minsucc rho in

    (* let temp = Mlglu.mdd_and minsucc incr_rho 1 1 in
       let temp = Mlglu.mdd_smooth_list mgr temp [rho] in
       let temp = Mlglu.mdd_substitute_two_lists mgr temp [rho'] [rho] in
    (* add the term for rho = max_val *)
       let keep_value = Mlglu.mdd_and minsucc rho_max 1 1 in
       Mlglu.mdd_or temp keep_value 1 1 
       in *)

    (* debug minsucc "minsucc";
       debug incr_minsucc "incr"; *)

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
      (* take maximum with respect to old Input measure *)
      let temp = Mlglu.mdd_or res mI 1 1 in

      Mlglu.mdd_max temp rho
    end else
      res
  in

  let mI     = ref rho_zero in
  let new_mI = ref rho_zero in
  let mO     = ref rho_zero in
  let diff   = ref (Mlglu.mdd_one mgr) in
  let measure_vars = bli :: (blo :: var_list) in

  while not (Mlglu.mdd_is_zero !diff) do

    print_time "cycle starts:";

    (* update output measure *)
    mO     := liftO !mI !mO;
    (* update input measure *)
    new_mI := liftI !mI !mO;
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
  Mlglu.mdd_not losers

    
(** Performs the pre-computations that are useful to CpreI.
  In particular, computes the transition relations 
  for both players.
 *)
let cpreI_init sp sm =
  let mgr = Symprog.get_mgr sp in
  let (tauI, tauO) = io_transitions sp sm in
  (* variables we deal with *)
  let vars = Symmod.get_vars sm in
  let vars' = Symprog.prime_vars sp vars in
  (* Warning: I'm assuming that the sets vars and vars' are converted
     into lists in the same order. Is this safe? Marco *)
  let var_list = Vset.to_list vars in
  (* let var_list' = Vset.to_list vars' in *)
  let var_list'' = Symprog.get_extra_vars sp var_list in 
  let vars'' = Vset.from_list var_list'' in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (mgr, tauI, tauO, bli, blo, vars, vars', vars'')


(** Performs the pre-computations that are useful to CpreO.
  In particular, computes the transition relations 
  for both players.
 *)
let cpreO_init sp sm =
  let mgr = Symprog.get_mgr sp in
  let (tauO, tauI) = oi_transitions sp sm in
  (* variables we deal with *)
  let vars = Symmod.get_vars sm in
  let vars' = Symprog.prime_vars sp vars in
  (* Warning: I'm assuming that the sets vars and vars' are converted
     into lists in the same order. Is this safe? Marco *)
  let var_list = Vset.to_list vars in
  (* let var_list' = Vset.to_list vars' in *)
  let var_list'' = Symprog.get_extra_vars sp var_list in 
  let vars'' = Vset.from_list var_list'' in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (mgr, tauO, tauI, blo, bli, vars, vars', vars'')


(** Computes the controllable predecessor operator for either player.

  The first argument provides the required predicates.
  If the first argument is the result of cpreI_init, we obtain CpreI;
  If the first argument is the result of cpreO_init, we obtain CpreO.
 *)
let cpre stuff m =
  (* variables are named as if they came from cpreI_init *)
  let (mgr, tauI, tauO, bli, blo, vars, vars', vars'') = stuff in
  let implication = Mlglu.mdd_or tauO m 0 1 in
  let term = Mlglu.mdd_consensus_list mgr implication [blo] in
  let term = Mlglu.mdd_consensus      mgr term vars in
  let term = Mlglu.mdd_and tauI term 1 1 in
  let term = Mlglu.mdd_smooth_list mgr term [bli] in
  let term = Mlglu.mdd_smooth      mgr term vars' in
  let term = Mlglu.mdd_smooth      mgr term vars'' in
  term


(** Computes the set of states where Input does not have a strategy
    to let time diverge or blame the adversary.
    Uses the algorithm based on a triple fixpoint. *)
let live_cpre input sp ?(verbose : bool = false) sm =
  let mgr = Symprog.get_mgr sp in

  (* variables for blameI and blameO *)
  let bli = Symprog.get_bli sp in
  let blo = Symprog.get_blo sp in
  (* literals for blameI and blameO *)
  let blitrue = Mlglu.mdd_literal mgr bli [1] in
  let blotrue = Mlglu.mdd_literal mgr blo [1] in

  let (stuff, color0, color1, color2) =
    if input then
      ( cpreI_init sp sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      Mlglu.mdd_and blitrue blotrue 1 0,
      blotrue )
    else
      ( cpreO_init sp sm,
      Mlglu.mdd_and blitrue blotrue 0 0,
      blotrue,
      Mlglu.mdd_and blitrue blotrue 1 0 )
  in

  let my_cpre = cpre stuff in

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
      done;

      y_diff := Mlglu.mdd_and !x !y 1 0;
      y := !x;
    done;
    
    z_diff := Mlglu.mdd_and !z !y 1 0;
    z := !y;
  done;

  print_time "end:";
  !z

(** Exported functions *)
let i_live = live_cpre true
let o_live = live_cpre false
let i_live_alt = i_live_internal ~gap:true


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
