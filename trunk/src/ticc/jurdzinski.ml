open Vset;;


(** Returns the set of states which have a measure which
    is separated from 0.
    In other words, states that have a measure value "m > 0"
    such that there is a value "n < m" such that
    no state has value n. *)
let separated sp measure vars rho =
  let mgr = Mlglu.mdd_get_manager measure in
  let not_m = Mlglu.mdd_not measure in
  let always_not_m = Mlglu.mdd_consensus_list mgr not_m vars in
  let min_gap = Mlglu.mdd_min mgr always_not_m rho in

  let tmp = Mlglu.mdd_get_support mgr min_gap in
  Symutil.print_varset_rough sp tmp;
  Mlglu.mdd_print mgr min_gap;
  ()


(** Computes the losing set of the non-Zenoness parity game.
    Refer to the techrep for details. *)
let winI sp sm =
  (* global data *)
  let mgr = Symprog.get_mgr sp in

  let (rho, rho') = Symprog.get_jurdzinski_var sp sm in
  let max_val = (Mlglu.mdd_get_var_range mgr rho) -1 in
  let rho_max = Mlglu.mdd_literal mgr rho [max_val] in
  let rho_zero = Mlglu.mdd_literal mgr rho [0] in
  Printf.printf "max value of rho=%d \n" max_val;
  flush stdout;

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
  let vars'' = Vset.from_list var_list'' in

  (* some popular predicates *)
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let iinv' = Mlglu.mdd_substitute_two_lists mgr 
    iinv var_list var_list' in
  let oinv' = Mlglu.mdd_substitute_two_lists mgr 
    oinv var_list var_list' in
  let delta1 = Symmod.get_delta1 sm in
  (* add the constraint that non-clock variables keep their value *)
  let delta1' = Symutil.and_unchngd sp delta1 not_cvars in
  let delta1_and_iinv = Mlglu.mdd_and delta1' iinv' 1 1 in

  let debug_jurdzinski = false in
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
  in

  (* the input transition relation *)
  let tauI =
    let copy_x_x'  = Symutil.unchngd sp vars in
    let copy_x_x'' = Mlglu.mdd_substitute_two_lists mgr 
      copy_x_x' var_list' var_list'' in
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_irules sm do_one_rule;
    (* conjoin with output invariant *)
    trans := Mlglu.mdd_and !trans oinv' 1 1;
    (* rename variables *)
    trans := Mlglu.mdd_substitute_two_lists mgr !trans var_list' var_list'';

    let term_action = Mlglu.mdd_and blitrue !trans 1 1 in
    let term_delta0 = Mlglu.mdd_and blitrue copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_smooth mgr delta1_and_iinv vars' in
    let term_delta1 = Mlglu.mdd_and term_delta1 copy_x_x'' 1 1 in
    let term_delta1 = Mlglu.mdd_and term_delta1 blitrue 1 0 in

    debug term_action "term_action";
    debug term_delta0 "term_delta0";
    debug term_delta1 "term_delta1";

    let res = Mlglu.mdd_or term_delta0 term_delta1 1 1 in
    let res = Mlglu.mdd_or res term_action 1 1 in
    Mlglu.mdd_and res copy_x_x' 1 1
  in

  (* the output transition relation *)
  let tauO =
    let copy_x_x'  = Symutil.unchngd sp vars in
    let copy_x'_x'' = Mlglu.mdd_substitute_two_lists mgr 
      copy_x_x' var_list var_list'' in
    let trans = ref (Mlglu.mdd_zero mgr) in
    let do_one_rule r : unit =
      let m = Ops.get_transition_rel_noinv sp sm r in
      trans := Mlglu.mdd_or !trans m 1 1;
    in
    Symmod.iter_lrules sm do_one_rule;
    Symmod.iter_orules sm do_one_rule;
    (* conjoin with output invariant *)
    trans := Mlglu.mdd_and !trans oinv' 1 1;

    let term1 = Mlglu.mdd_or copy_x'_x'' !trans 1 1 in
    let term1 = Mlglu.mdd_and term1 blotrue 1 1 in
    let term2 = Mlglu.mdd_and blitrue blotrue 1 0 in
    let term2 = Mlglu.mdd_and term2 copy_x'_x'' 1 1 in
    let term3 = Mlglu.mdd_and blitrue blotrue 0 0 in
    let term3 = Mlglu.mdd_and term3 delta1_and_iinv 1 1 in
    let term3 = Mlglu.mdd_and term3 oinv' 1 1 in
    let res = Mlglu.mdd_or term1 term2 1 1 in
    Mlglu.mdd_or res term3 1 1
  in
  
  (** DEBUG **)
  debug tauI "tauI";
  debug tauO "tauO";

  let liftO (measure: Mlglu.mdd) : Mlglu.mdd =
    let measure' = Mlglu.mdd_substitute_two_lists mgr 
      measure var_list var_list' in
    let res = Mlglu.mdd_and tauO measure' 1 1 in
    let res = Mlglu.mdd_smooth_list mgr res [blo] in
    let res = Mlglu.mdd_smooth mgr res vars' in
    Mlglu.mdd_max mgr res rho
  in

  let liftI (measure: Mlglu.mdd) : Mlglu.mdd =
    let measure' = Mlglu.mdd_substitute_two_lists mgr 
      measure var_list var_list' in
    let res = Mlglu.mdd_and tauI measure' 1 1 in
    let res = Mlglu.mdd_smooth_list mgr res [bli] in
    let res = Mlglu.mdd_smooth mgr res vars' in
    let res = Mlglu.mdd_smooth mgr res vars'' in
    let minsucc = Mlglu.mdd_min mgr res rho in

    let incr m =
      let temp = Mlglu.mdd_eq_plus_c mgr rho' rho 1 in
      let temp = Mlglu.mdd_and m temp 1 1 in
      let temp = Mlglu.mdd_smooth_list mgr temp [rho] in
      let temp = Mlglu.mdd_substitute_two_lists mgr temp [rho'] [rho] in
      (* add the term for rho = max_val *)
      let keep_value = Mlglu.mdd_and m rho_max 1 1 in
      Mlglu.mdd_or temp keep_value 1 1 
    in
    (* slices of the parity games *)
    let color0 = Mlglu.mdd_and blitrue blotrue 0 0 in
    let color1 = Mlglu.mdd_and blitrue blotrue 1 0 in
    let color2 = blotrue in

    debug minsucc "minsucc";
    debug (incr minsucc) "incr";

    let term0 = Mlglu.mdd_and rho_max minsucc 1 1 in
    let temp = Mlglu.mdd_lt_c mgr rho max_val in
    let temp = Mlglu.mdd_and temp minsucc 1 1 in
    let temp = Mlglu.mdd_smooth_list mgr temp [rho] in
    let temp = Mlglu.mdd_and temp rho_zero 1 1 in
    let term0 = Mlglu.mdd_or term0 temp 1 1 in
    let term0 = Mlglu.mdd_and term0 color0 1 1 in

    let term1 = Mlglu.mdd_and color1 (incr minsucc) 1 1 in

    let term2 = Mlglu.mdd_and color2 minsucc 1 1 in
    let res = Mlglu.mdd_or term0 term1 1 1 in
    Mlglu.mdd_or res term2 1 1
  in

  let measure = ref rho_zero in
  let diff = ref (Mlglu.mdd_one mgr) in
  let measure_vars = bli :: (blo :: var_list) in

  while not (Mlglu.mdd_is_zero !diff) do
    let new_measure = liftI (liftO !measure) in

    print_string ".";

    separated sp new_measure measure_vars rho;
    (* let debug_mdd = Mlglu.mdd_not new_measure in
       let debug_mdd = Mlglu.mdd_consensus_list mgr debug_mdd [rho] in
       if not (Mlglu.mdd_is_zero debug_mdd) then begin
       debug_always debug_mdd "states with no measure";
       raise Not_found;
       end; *)
    
    diff := Mlglu.mdd_and new_measure !measure 1 0;
    measure := new_measure;
  done;
  print_string "\n";
  let losers = Mlglu.mdd_and !measure rho_max 1 1 in
  let losers = Mlglu.mdd_smooth_list mgr losers [rho] in
  losers

