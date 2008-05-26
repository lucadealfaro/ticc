(** This is the main module of ticc, defining all the functions that
    are available to the user at the toplevel. *)

(** This is the type of a symbolic representation of a set of
    states. *)
type stateset_t = Symmod.stateset_t

(** This is the type of a symbolic module. *)
type symbolic_module_t = Symmod.t 

(** Indicates that the operation has not yet been implemented for
    timed modules. *) 
exception NoTimedSupport = Ops.NoTimedSupport

(** This exception can be thrown by [compose] to indicate that the
    modules are not composable. *)
exception Modules_not_composable = Compose.Modules_not_composable

(** this exception can be thrown by [compose] to indicate that the
    modules being composed are incompatible. *)
exception Incompatible_Modules = Compose.Incompatible_Modules

(** Raises an exception if [sm] is timed. *)
let assert_untimed (sm: Symmod.t) : unit =
  if Symmod.is_timed sm then
    begin
      Printf.printf "Operation not supported on timed modules.\n";
      raise NoTimedSupport
    end;
;;

(** Parses a file. 
    When parse aborts because of an error, all the parsed stuff is
    lost, so that you can try to parse the file again after correcting
    the problem. *)
let parse (f: string) = 
  let cin = open_in f in 
  let lexbuf = Lexing.from_channel cin in
  Ticparse.design Ticlex.token lexbuf;
  (* copy the new declarations to toplevel *) 
  Prog.fold_to_top ()
  
(** Given the name of a module, 
    it builds and returns the symbolic representation of the module. 
*)
let mk_sym = Modops.mk_sym


(** Given the name of a stateset, build a symbolic representation for
    it. *) 
let mk_set (set_name: string) = 
  let a = 
    try Prog.get_sset_top set_name
    with Not_found -> 
      Printf.printf "\n Error: Unknown stateset!\n";
      raise Not_found
  in
  let st = Symbuild.mk_bool Symprog.toplevel a in 
  (* For some reason, it does not seem to be necessary to add st to
     the symbolic top level... go figure. *) 
  st

(** Given a string containing an expression, returns the symbolic
    representation of the set of states that satisfy the
    expression. *)
let parse_stateset (exp_string: string) : stateset_t =
  Symbuild.parse_stateset Symprog.toplevel exp_string

(** clones an enumerative module *)
let clone = Prog.clone_modp Prog.toplevel;;

(** clones a symbolic module *)
let sym_clone (m: Symmod.t) : Symmod.t = 
  let mgr = Symprog.get_mgr Symprog.toplevel in 
  Symmod.symbolic_clone mgr m

(** compose two modules *)
let compose ?(result_name:string = "") sm1 sm2 = 
  let win_algo = if ((Symmod.is_timed sm1) or (Symmod.is_timed sm2)) then 
    Zeno.win_composition
  else
    Ops.win_i_safe
  in
  Compose.composition Symprog.toplevel win_algo ~result_name sm1 sm2;;

let compose_alt = Compose.composition Symprog.toplevel Ops.win_i_safe_alt_1;;

(** Computes the local/output post of a module [sm] 
    and a stateset [set]. *)
let lo_post (sm: Symmod.t) (set: stateset_t) : stateset_t =
  Ops.lo_post Symprog.toplevel sm set true
 
(** Computes the input post of a module [sm] and a stateset [set]. *)
let i_post (sm: Symmod.t) (set: stateset_t) : stateset_t =
  Ops.i_post Symprog.toplevel sm set true

(** Computes the post of a module [sm] and a stateset [set]. *)
let post (sm: Symmod.t) (set: stateset_t) : stateset_t =
  Ops.loi_post Symprog.toplevel sm set

(** Computes the set of winning states for player Input w.r.t.
  the safety goal [set]. *)
let win_i_safe (sm: Symmod.t) (set: stateset_t) 
  : stateset_t  = 
  Ops.win_i_safe Symprog.toplevel sm set
  ;;

(** Computes the set of winning states for player Output w.r.t.
  the safety goal [set]. *)
let win_lo_safe (sm: Symmod.t) (set: stateset_t) 
  : stateset_t  = 
  Ops.win_lo_safe Symprog.toplevel sm set
  ;;

(** Returns the MDD representing the input invariant of module m *)
let get_iinv m = Symmod.get_iinv m 

(** Returns the MDD representing the output invariant of module m *)
let get_oinv m = Symmod.get_oinv m 

(** Returns the MDD representing the initial states of the module m *)
let get_initial m = Symmod.get_init m

(** Sets the initial condition of a module *)
let set_initial = Modops.set_new_init_cond

(** [get_stateset m sn] returns the MDD representing the stateset
    (set of states) with name [sn] declared in module [m]. *)
let get_stateset m sn = 
  try Symmod.get_sset m sn 
  with Not_found -> 
    Printf.printf "\n Error: Unknown stateset %s!\n" sn;
    raise Not_found

(** Returns the reachable states *)
let get_reachable m = Ops.reachable Symprog.toplevel m 

(** Returns an input restriction specifying how a rule [r] of a
    symbolic module [sm] is restricted by the input invariant. *)
let get_input_restriction sm (r: string) = 
  Modops.get_input_restriction Symprog.toplevel sm r 

(** Prints [n_traces] restriction paths for rule [r] of module [sm],
    leading from the initial condition, to an istance of input rule
    [r], to the set of bad states of [sm]. *)
let print_restr_paths sm (r: string) (n_traces: int) = 
  Modops.print_n_restriction_paths Symprog.toplevel sm r n_traces

(** Closes a module wrt. an action *)
let close m a = Modops.close_input_action Symprog.toplevel m a 

(** Returns the most general refinement relation between [m1] and [m2] *)
let refinement m1 m2 = 
  assert_untimed m1;
  assert_untimed m2;
  Refine.refinement Symprog.toplevel m1 m2 

(** Checks whether [m1] refines [m2] *)
let refines m1 m2 = 
  assert_untimed m1;
  assert_untimed m2;
  Refine.refines Symprog.toplevel m1 m2 

(** Returns the set of I-live states *)
let i_live = Timedabs.i_live Symprog.toplevel 

(** Returns the set of O-live states *)
let o_live = Timedabs.o_live Symprog.toplevel 

(** Returns the set of I-live states *)
let i_live_alt = Zeno.i_live_alt Symprog.toplevel 

(** Forgets a module *)
let forget = Modops.forget_module Symprog.toplevel 

(** DEBUG function: unwraps an mdd *)
let unwrap_stateset (s: stateset_t) : Mlglu.mdd =
  Symmod.unwrap_stateset s

(** DEBUG function: wraps an mdd *)
let wrap_stateset (s: Mlglu.mdd) : stateset_t = 
  Symmod.wrap_stateset s

(** Random simulates a module, given its initial state as a ticc
    expression for the given number of cycles. *)
let simulate (sm: Symmod.t) (expr: string) (nCycles: int)
    (outFile: string) : unit =
  assert_untimed sm;
  Simulate.simulate sm expr nCycles outFile
;;

(** Comparisons between mdds *)

let set_equal b1 b2 = Mlglu.mdd_equal b1 b2;;

let set_is_subset b1 b2 = Mlglu.mdd_is_one (Mlglu.mdd_or b1 b2 0 1)

(** Logical operations between BDDs *) 
let set_is_empty b = Mlglu.mdd_is_zero b
let set_is_nonempty b = not (Mlglu.mdd_is_zero b)
let set_and  b1 b2 = Mlglu.mdd_and b1 b2 1 1
let set_or   b1 b2 = Mlglu.mdd_or  b1 b2 1 1
let set_impl b1 b2 = Mlglu.mdd_or  b1 b2 0 1
let set_not  b     = Mlglu.mdd_not b

(** prints the top level *)
let print_toplevel () = 
  Prog.print_top (); 
  print_string "\n\n"

(** prints the top level *)
let print_debug_toplevel () = 
  Prog.print_debug_top (); 
  print_string "\n\n"

(** prints the set of states [x] *)
let print_stateset x =
  let mgr = Mlglu.mdd_get_manager x in
  flush stdout; 
  Mlglu.mdd_print mgr x; 
  flush stdout

(** prints the list of known modules (just the names) *)
let print_list_modules () = 
  Prog.list_modules_top (); 
  print_string "\n"

(** prints the global variables *) 
let print_vars () = 
  Prog.print_vars_top (); 
  print_string "\n"

(** prints a symbolic module *)
let print_symmod sm : unit =
  Symprint.print Symprog.toplevel sm
  ;;

(** prints a symbolic invariant of a module *)
let print_symmod_iinv = Symprog.print_iinv Symprog.toplevel;;
let print_symmod_oinv = Symprog.print_oinv Symprog.toplevel;;

(** prints how an input rule [r] of a symbolic module [sm]
    is restricted by the input invariant. *)
let print_input_restriction sm (r: string) = 
  Symprint.print_input_restriction Symprog.toplevel sm r 

(** print an enum module with debug info *) 
let print_module_full (mn: string) = 
  let m = Prog.get_mod_top mn in 
  Mod.print_debug m

(** prints the rules that correspond to an action *)
let print_symmod_rules = Symprint.print_rulemodact Symprog.toplevel;;

(** Prints a boolean value *)
let print_bool b = 
  if b 
  then print_string "true"
  else print_string "false"

(* **************************************************************** *)
(* CTL *)

(* Existential *)

let ctl_e_until (sm: symbolic_module_t) 
	(b: stateset_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.e_until Symprog.toplevel sm b r

let ctl_e_waitfor (sm: symbolic_module_t) 
	(b: stateset_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.e_waitfor Symprog.toplevel sm b r 

let ctl_e_f (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  let mgr = Symprog.get_mgr Symprog.toplevel in 
  let one = Mlglu.mdd_one mgr in 
  Ops.e_until Symprog.toplevel sm one r

let ctl_e_g (sm: symbolic_module_t) (b: stateset_t) : stateset_t = 
  assert_untimed sm;
  let mgr = Symprog.get_mgr Symprog.toplevel in 
  let zero = Mlglu.mdd_zero mgr in 
  ctl_e_waitfor sm b zero 
    
let ctl_e_next (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.e_pre Symprog.toplevel sm r 

let ctl_e_input_next (sm: symbolic_module_t) (r: stateset_t) :
  stateset_t = 
  assert_untimed sm;
  Ops.e_input_pre Symprog.toplevel sm r 

let ctl_e_output_next (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.e_output_pre Symprog.toplevel sm r 

(* Universal *)

(** Uses: \forall \box B = \not \exists \diam \not B *)
let ctl_a_g (sm: symbolic_module_t) (b: stateset_t) : stateset_t = 
  let result = Mlglu.mdd_not (ctl_e_f sm (Mlglu.mdd_not b)) in 
  Ops.conjoin_w_invs sm result

(** Uses: \forall (B \waitfor R) 
    = \not \exists (\not R \Until (\not R \inters \not B)) *)
let ctl_a_waitfor (sm: symbolic_module_t) 
	(b: stateset_t) (r: stateset_t) : stateset_t = 
    let not_b = Mlglu.mdd_not b in 
    let not_r = Mlglu.mdd_not r in 
    let not_b_and_not_r = Mlglu.mdd_and not_b not_r 1 1 in 
    let result = Mlglu.mdd_not (ctl_e_until sm not_r not_b_and_not_r) in 
    Ops.conjoin_w_invs sm result

(** Uses: \forall (B \until R) 
    = \not \exists (\not R \waitfor (\not R \inters \not B)) *)
let ctl_a_until (sm: symbolic_module_t) 
	(b: stateset_t) (r: stateset_t) : stateset_t = 
    let not_b = Mlglu.mdd_not b in 
    let not_r = Mlglu.mdd_not r in 
    let not_b_and_not_r = Mlglu.mdd_and not_b not_r 1 1 in 
    let result = Mlglu.mdd_not (ctl_e_waitfor sm not_r not_b_and_not_r) in 
    Ops.conjoin_w_invs sm result

let ctl_a_f (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
    let mgr = Symprog.get_mgr Symprog.toplevel in 
    let one = Mlglu.mdd_one mgr in 
    ctl_a_until sm one r

let ctl_a_next (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.apre Symprog.toplevel sm r

let ctl_a_input_next (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.i_apre Symprog.toplevel sm r

let ctl_a_output_next (sm: symbolic_module_t) (r: stateset_t) : stateset_t = 
  assert_untimed sm;
  Ops.lo_apre Symprog.toplevel sm r

let ctl_and = set_and
let ctl_or  = set_or

let ctl_not sm b = 
    Ops.conjoin_w_invs sm (set_not b)
    
(**********************abstraction refinement**********************)
let abstract_ctl_e_f = Abstract.ctl_e_f
(*let create_initial_abstract_module = Abstract.create_initial_abstract_module
let compute_abstract_cpre = Abstract.compute_abstract_cpre*)
let ctl_e_f_init (sm: symbolic_module_t)  (init: stateset_t) (r: stateset_t): bool = 
  assert_untimed sm;
  let mgr = Symprog.get_mgr Symprog.toplevel in 
  let one = Mlglu.mdd_one mgr in 
  let setv = Ops.e_until Symprog.toplevel sm one r in
  let initset = set_and  setv init in
  if Mlglu.mdd_is_zero initset then false else true 

