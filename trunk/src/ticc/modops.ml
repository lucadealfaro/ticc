(** This module contains operations on modules as a whole *)

type stateset_t = Mlglu.mdd;;
module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 

exception Internal_error_waypoint

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
  let m = 
    try Prog.get_mod_top mod_name
    with Not_found ->
      Printf.printf "\nError: Unknown module %s!\n" mod_name;
      raise Not_found
  in
  (* builds the symbolic module *)
  let sm = Symbuild.mk_mod m Symprog.toplevel in
  (* Remembers the complement of the original iinv in bad_states *)
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let not_iinv = Mlglu.mdd_not iinv in 
  Symmod.set_bad_states sm not_iinv; 

  (* Strengthen invariants to put module into normal form *)
  let (strong_iinv, strong_oinv) = 
    if Symmod.is_timed sm then
      (* ( iinv, oinv ) *)
      ( Zeno.i_live Symprog.toplevel sm, 
      Zeno.o_live Symprog.toplevel sm ) 
    else
      ( Ops.win_i_safe  Symprog.toplevel sm (Symmod.get_iinv sm),
      Ops.win_lo_safe Symprog.toplevel sm (Symmod.get_oinv sm) )
  in
  if (not (Mlglu.mdd_is_zero (Mlglu.mdd_and iinv strong_iinv 1 0))) then begin
      Printf.printf "\nThe input invariant of module %s was modified (strengthened) to ensure well-formedness.\n" mod_name; 
    flush stdout
  end; 
  if (not (Mlglu.mdd_is_zero (Mlglu.mdd_and oinv strong_oinv 1 0))) then begin
      Printf.printf "\nThe output invariant of module %s was modified (strengthened) to ensure well-formedness.\n" mod_name; 
    flush stdout
  end; 
  Symmod.set_iinv sm strong_iinv;
  Symmod.set_oinv sm strong_oinv;

  (* Update the initial condition *)
  let new_init = Mlglu.mdd_and (Mlglu.mdd_and (Symmod.get_init sm)
    strong_iinv 1 1) strong_oinv 1 1 in 
  Symmod.set_init sm new_init; 

  if Mlglu.mdd_is_zero new_init then begin
    Printf.printf "\nThe module %s has an empty set of initial states!\n" mod_name; 
    flush stdout
  end; 
  (** the result *)
  sm
;;

(* **************************************************************** *)
(*                                                                  *)
(*  Closes an action for a module                                   *)
(*                                                                  *)
(* **************************************************************** *)
(** Closes an action for a module: says that no-one is any longer able
    to send that action to the module.  This can be used, after
    several modules all outputting/inputting a certain action have
    been composed, to say that the environment is no longer able to
    provide that action. 
    The implementation is as follows. 
    
    - If the module already contains the action, its transition relation is set to false. 

    - If the module does not contain the action, it is added to the
      module, with a false input transition relation.  

    This works even in the case of actions with wildcards. 
    Returns the new module. 
*)

let close_input_action (sp: Symprog.t) (sm: Symmod.t) (a_name: string) : Symmod.t =
  let mgr = Symprog.get_mgr sp in 
  (* Clones the module *)
  let new_sm = Symmod.symbolic_clone mgr sm in 
  Symutil.mk_false_irule sp new_sm a_name; 
  (* We need to erase the reachset, as it might have changed *)
  Symmod.set_reachset new_sm None;
  (* In case of timed modules, we need also to recompute the I-live states *)
  if Symmod.is_timed new_sm then begin
    let strong_iinv = Ops.win_i_safe  Symprog.toplevel new_sm (Symmod.get_iinv sm) in 
    let new_init = Mlglu.mdd_and (Symmod.get_init new_sm) strong_iinv 1 1 in 
    Symmod.set_iinv new_sm strong_iinv;
    if Mlglu.mdd_is_zero new_init then begin
      Printf.printf "\nAfter closing, the module has an empty set of initial states!\n"; 
      flush stdout
    end
  end;  
  (** the result *)
  new_sm
;;

(* **************************************************************** *)
(*  Forgets a module                                                *)
(* **************************************************************** *)

(** This apparently silly function enables the garbage collector to
    collect all the MDDs associated with the module.  Warning: strange
    things might happen if you then try to use the module! Use with
    care!  If you wonder why this is here, rather than in symmod.ml
    where it belongs - well, in symmod we cannot get the mdd manager,
    as symprog is above symmod, not below it, in the hierarchy.  Crazy
    eh?
*)
let forget_module (sp: Symprog.t) (sm: Symmod.t) : unit = 
  let mgr =  Symprog.get_mgr sp in
  let mdd1 = Mlglu.mdd_one mgr in 
  Symmod.set_init sm mdd1; 
  Symmod.set_reachset sm None; 
  Symmod.clear_ssets sm; 
  Symmod.set_iinv sm mdd1; 
  Symmod.set_oinv sm mdd1; 
  Symmod.set_old_iinv sm mdd1;
  Symmod.set_bad_states sm mdd1;
  Symmod.clear_lrules sm; 
  Symmod.clear_irules sm; 
  Symmod.clear_orules sm
;;

(* **************************************************************** *)
(*  Analyzes input restriction                                      *)
(* **************************************************************** *)


(** Gets an input restriction *)
let get_input_restriction sp sm (r: string) = 
  let mgr = Symprog.get_mgr sp in
  let iinv = Symmod.get_iinv sm in
  let old_iinv = Symmod.get_old_iinv sm in 
  (* The restriction is something that: 
     1. starts from a reachable state
     2. starts from a state satisfying iinv
     3. leads to a state satisfying old_iinv but not iinv
     4. follows the rule 
     This is an example of something that used to be possible, 
     but no longer is. *)
  if Symmod.has_iaction sm r then begin 
    (* ir is the input rule we work on *)
    let ir = Symmod.get_irule sm r in 
    let tr = Ops.get_transition_rel_noinv sp sm ir in 
    (* Computes the set of reachable states of the module *)
    let reachset = Ops.reachable sp sm in 
    (* Now builds the answer: restr = tr /\ iinv /\ reachset /\ not iinv' /\ old_iinv' *)
    let iinv' = Symutil.prime_mdd sp sm iinv in
    let old_iinv' = Symutil.prime_mdd sp sm old_iinv in 
    let set_start = Mlglu.mdd_and iinv  reachset  1 1 in 
    let set_end   = Mlglu.mdd_and iinv' old_iinv' 0 1 in 
    let restr     = Mlglu.mdd_and (Mlglu.mdd_and set_start tr 1 1) set_end 1 1 in
    (* At this point, what is left to do is to quantify out
       all primed local variables, since anyway their update
       is deterministic. *) 
    let local_variables' = Symprog.prime_vars sp (Symmod.get_lvars sm) in
    let result = Mlglu.mdd_smooth mgr restr local_variables' in
    result
  end else begin
    Printf.printf "\nNo input action named %s in the module.\n" r;
    raise Not_found
  end
;;

(** Given: 
    [start_set]: starting set of states
    [end_set]: ending set of states
    [by_set]: a set of possible intermediate states
    [use_input]: whether to use input transitions
    [use_output]: whether to use output transitions 
    [go_back]: goes backwards?  otherwise, forwards. 
    This function returns a list of cubes, c0, c1, ..., cn, starting in 
    [start_set], ending in [end_set], by [by_set], and using input and
    output rules as requested.  The list of cubes can be 
    empty, indicating that no such path exists. *)
let find_cube_path sp sm (start_set: Mlglu.mdd) (end_set: Mlglu.mdd) 
    (by_set: Mlglu.mdd) (use_input: bool) (use_output: bool) = 
  let vAll = Symmod.get_vars sm in
  let mgr = Symprog.get_mgr sp in

  (* sanity checks *)
  Symutil.assert_no_primed sp start_set; 
  Symutil.assert_no_primed sp end_set; 
  Symutil.assert_no_primed sp by_set; 

  (* does it forwards *)
  (* Keeps the onion ring structure *)
  let onion = ref [] in 
  let last_set = ref start_set in 
  (* goes forwards until it hits the end set *)
  let inters = ref (Mlglu.mdd_and !last_set end_set 1 1) in 

  while Mlglu.mdd_is_zero !inters do
    (* Needs to go forward one more step *)
    onion := !last_set :: !onion;
    last_set := Mlglu.mdd_and (Ops.raw_post use_input use_output sp sm !last_set) by_set 1 1; 
    Symutil.assert_no_primed sp !last_set; 
    inters := Mlglu.mdd_and !last_set end_set 1 1
  done; 
  (* Fine, now it must go backwards producing the cubes *)
  (* gets a cube in inters *)
  Symutil.assert_no_primed sp !inters; 
  let cube = Mlglu.mdd_pick_one_cube mgr !inters vAll in 
  
  (* Given a cube c, it look for a cube c' in the intersection of the
     pre of c, and of the head of the list l.  Then, attaches c to the 
     answer, and calls itself on c' and the tail of the list. *)
  (* NOTE: todo: here, it would be better to pick not only a cube, but
     also the rule that produced it.  This would lead to much better
     diagnostic output.  I think the simplest is to: 
     1. check which rule led to the transition (taking care of use_input, use_output)
     2. choose a cube produced by that transition.
     The action for the rule has to be printed as part of the trace. *)
  let rec pick_cubes (c: Mlglu.mdd) (l: Mlglu.mdd list) = 
    match l with 
	[] -> [c]
      | s :: l' -> begin
	  (* p is the predecessor of cube c with intersection to s *)
	  (* Note that I do not need to worry about by_set, as it is
	     already taken into account into the elements of l *)
	  let p = Mlglu.mdd_and s (Ops.internal_pre use_input use_output sp sm c) 1 1 in 
	  (* gets a cube in p *)
	  Symutil.assert_no_primed sp p; 
	  let c' = Mlglu.mdd_pick_one_cube mgr p vAll in 
	  c :: (pick_cubes c' l')
	end
  in
  List.rev (pick_cubes cube !onion)
;;

(** The following is a random number generator. This code has been
    pinched from www.bagley.org/~doug/shootout
*)

let lastRef = ref 42;;

let generateRandom (range: int) : int =
  let random (max: int) =
    let im = 139968
    and ia = 3877
    and ic = 29573
    in
    let newLast = (!lastRef * ia + ic) mod im in
      lastRef := newLast;
      float_of_int max *. float_of_int newLast /. float im;
  in
  let rec loop (i: int) =
    let r = random 100 in
      if i > 1 then loop (i - 1) else r
  in
    (int_of_float(loop range) mod range)
;;

(** The following function is used to interpolate actions that
    witness a path through a given list of cubes *)

let computeNextState (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) 
    (cube: stateset_t) (outputsOnly: bool) : 
    (stateset_t * Symmod.rule_t) option =
  let mgr = Symprog.get_mgr sp in
  let zero = Mlglu.mdd_zero mgr in
  let rules = ref [] in
  let doOneRule (r: Symmod.rule_t) : unit =
    let dest = Ops.post_rule sp sm set r false in
      if not (Mlglu.mdd_equal dest zero) then
	let intersect = Mlglu.mdd_and dest cube 1 1 in
	  if not (Mlglu.mdd_equal intersect zero) then
	    rules := r::!rules;
  in
    if not (outputsOnly) then
      Symmod.iter_irules sm doOneRule;
    Symmod.iter_orules sm doOneRule;
    let nRules = List.length !rules in
      if (nRules = 0) then  begin
	Printf.printf "No rules found!!\n";
	None;
      end else
	let random = generateRandom(nRules) in
	  Some((Ops.post_rule sp sm set (List.nth !rules random) false), 
	       (List.nth !rules random));
;;

(** Pring the counterexample path in html format and as an ascii text file *)

let printCounterExample (sp: Symprog.t) (sm: Symmod.t) 
    (path1: stateset_t list) (path2: stateset_t list)
    (rul: string) (filename: string) : unit =
  let ir = Symmod.get_irule sm rul in
  let startState = Symmod.get_init sm in
  let len = List.length path1 in 
  let lastCube = List.nth path1 (len - 1) in 
  let dest = Ops.post_rule sp sm lastCube ir false in 
  let outChannel: out_channel = open_out filename in
  let interpolateActions : unit =
    Htmlgen.generateHtmlHeader sp sm startState "Counterexample Trace" outChannel;
    let prevState = ref startState in
    let doOneCube (outputsOnly: bool) (cube: Mlglu.mdd) : unit =
      match (computeNextState sp sm !prevState cube outputsOnly) with
	  Some(state, r) ->
	    Htmlgen.generateHtmlCycle sm r !prevState state outChannel None;
	    prevState := state;
	| None -> ()
    in
      List.iter (doOneCube false) path1;
      (* Here is where the input rule is taken *)
      Htmlgen.generateHtmlCycle sm ir !prevState dest outChannel (Some(ir));
      prevState := dest;
      let skipList : stateset_t list = 
	match path2 with
	    first::rest -> rest
	  | [] -> path2
      in
	List.iter (doOneCube true) skipList;
  in
    interpolateActions;
    Htmlgen.generateHtmlFooter outChannel;
;;

(** Prints the counterexample path *)
let print_counterex_rule sp sm (path1: Mlglu.mdd list) (path2: Mlglu.mdd list)
    (rul: string) : unit =
  let mgr = Symprog.get_mgr sp in 
  Printf.printf "\nCounterexample path to rule %s:\n" rul; 
  flush stdout; 
  List.iter (Mlglu.mdd_print mgr) path1;
  flush stdout; 
  Printf.printf "\nHere rule %s is taken\n" rul; 
  flush stdout; 
  List.iter (Mlglu.mdd_print mgr) path2;
  flush stdout; 
  Printf.printf "\nHere the set of bad states has been reached.\n";
;;

(** Prints [n_traces] restriction paths for rule [r] in module [sm]. *)
let print_n_restriction_paths sp sm (rul: string) (n_traces: int) =
  let vAll  = Symmod.get_vars sm in
  let vAll' = Symprog.prime_vars sp vAll in 
  let mgr = Symprog.get_mgr sp in
  (* first, computes the unprimed restriction *)
  let restr = get_input_restriction sp sm rul in 
  let unp_restr = Mlglu.mdd_smooth mgr restr vAll' in 
  (* ir is the input rule we work on *)
  let ir = Symmod.get_irule sm rul in 

  (* Now it gets at most n_traces disjoint cubes out of it, 
     and prints the corresponding traces. *)
    if Mlglu.mdd_is_zero unp_restr then 
      Printf.printf "\nNo restriction for rule %s\n" rul 
    else begin 
      let i = ref n_traces in 
      let r = ref unp_restr in 
	while !i > 0 &  not (Mlglu.mdd_is_zero !r) do 
	  i := !i - 1; 
	  (* finds, forwards, a path from the initial condition to the place where 
	     the restriction has occurred.  The path has to be in iinv.  *)
	  let path1 = 
	    find_cube_path sp sm (Symmod.get_init sm) !r (Symmod.get_iinv sm) true true 
	  in 
	    
	  (* gets the last cube *)
	  let len = List.length path1 in 
	  let last_cube1 = List.nth path1 (len - 1) in 
	    Symutil.assert_no_primed sp last_cube1; 
	    
	    (* sanity check *)
	    if not (Mlglu.mdd_is_zero (Mlglu.mdd_and last_cube1 unp_restr 1 0))
	    then raise Internal_error_waypoint; 
	    
	    (* Applies the rule from last_cube1, computing the set dest after
	       the rule has been taken. *)
	    let dest = Ops.post_rule sp sm last_cube1 ir false in 
	      Symutil.assert_no_primed sp dest; 
	      (* Now finds another series of cubes, from dest, to the set of
		 bad states, via old_inv, using only output transitions *)
	      
	      let path2 = find_cube_path sp sm dest (Symmod.get_bad_states sm) 
		(Symmod.get_old_iinv sm) false true in
		
		(* Prints the two lists of cubes.  Keeps them separate, so we know 
		   where the rule is. *)
		print_counterex_rule sp sm path1 path2 rul; 

		let filename = (Symmod.get_name sm)^"_"^rul^"_"^string_of_int(!i)^".html" in
		  printCounterExample sp sm path1 path2 rul filename;

		(* Now subtracts last_cube1 from !r, as a path through last_cube1 
		   has been printed. *)
		r := Mlglu.mdd_and !r last_cube1 1 0; 
	done
    end
;;

(* **************************************************************** *)
(*  Modifies the initial condition of a module                      *)
(* **************************************************************** *)

let set_new_init_cond (sm: Symmod.t) (new_init: Mlglu.mdd) : unit = 
  (* Sets the new initial condition *)
  Symmod.set_init sm new_init; 
  (* Now it has to say that the set of reachable states is no longer known *)
  Symmod.erase_what_known sm
;;
