(** This is the main module of ticc, defining all the functions that
    are available to the user at the toplevel. *)

(** This is the type of a symbolic representation of a set of
    states. *)
type state_set_t = Symops.stateset_t

(** This is the type of a symbolic module. *)
type symbolic_module_t = Symmod.t 

(** Indicates that the operation has not yet been implemented for
    timed modules. *) 
exception NoTimedSupport = Symops.NoTimedSupport

(** This exception can be thrown by [compose] to indicate that the
    modules are not composable. *)
exception Modules_not_composable = Symops.Modules_not_composable

(** this exception can be thrown by [compose] to indicate that the
    modules being composed are incompatible. *)
exception Incompatible_Modules = Symops.Incompatible_Modules

(** compose two modules *)
let compose = Symops.composition Symprog.toplevel Symops.win_i_safe;;
let compose_alt = Symops.composition Symprog.toplevel Symops.win_i_safe_alt_1;;

(*
(** compose two lists of modules *)
let compose2opt = Symops.pairwise_composition Symprog.toplevel true Symops.win_i_safe;;
let compose2pes = Symops.pairwise_composition Symprog.toplevel false Symops.win_i_safe;;

(** calculate an input invariant based on pairwise composition *)
let calculate_inv = Symops.composition_test2 Symprog.toplevel Symops.win_i_safe;;
 *)

(** Computes the local/output post of a module [sm] 
    and a stateset [set]. *)
let lo_post (sm: Symmod.t) (set: Symops.stateset_t) : Symops.stateset_t =
  Symops.lo_post Symprog.toplevel sm set
 
(** Computes the input post of a module [sm] and a stateset [set]. *)
let i_post (sm: Symmod.t) (set: Symops.stateset_t) : Symops.stateset_t =
  Symops.i_post Symprog.toplevel sm set

(** Computes the set of winning states for player Input w.r.t.
  the safety goal [set]. *)
let win_i_safe (sm: Symmod.t) (set: Symops.stateset_t) 
  : Symops.stateset_t  = 
  Symops.win_i_safe Symprog.toplevel sm set
  ;;

(** Computes the set of winning states for player Output w.r.t.
  the safety goal [set]. *)
let win_lo_safe (sm: Symmod.t) (set: Symops.stateset_t) 
  : Symops.stateset_t  = 
  Symops.win_lo_safe Symprog.toplevel sm set
  ;;

(** Returns the MDD representing the input invariant of module m *)
let get_iinv m = Symmod.get_iinv m 

(** Returns the MDD representing the output invariant of module m *)
let get_oinv m = Symmod.get_oinv m 

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
let mk_sym = Symops.mk_sym


(** Given the name of a stateset, build a symbolic representation for
    it. *) 
let mk_set (set_name: string) = 
  let a = 
    try Prog.get_sset_top set_name
    with Not_found -> 
      Printf.printf " Error: Unknown stateset!";
      raise Not_found
  in
  let st = Symbuild.mk_bool Symprog.toplevel a in 
  (* For some reason, it does not seem to be necessary to add st to
     the symbolic top level... go figure. *) 
  st

(** Given a string containing an expression, returns the symbolic
    representation of the set of states that satisfy the
    expression. *)
let parse_stateset (exp_string: string) : Symops.stateset_t =
  Symbuild.parse_stateset Symprog.toplevel exp_string

(** clones an enumerative module *)
let clone = Prog.clone_modp Prog.toplevel;;

(** clones a symbolic module *)
let sym_clone (m: Symmod.t) : Symmod.t = 
  let mgr = Symprog.get_mgr Symprog.toplevel in 
  Symmod.symbolic_clone mgr m

(** Comparisons between mdds *)

let set_equal b1 b2 = Mlglu.mdd_equal b1 b2;;

let set_is_subset b1 b2 = Mlglu.mdd_is_one (Mlglu.mdd_or b1 b2 0 1)

(** Logical operations between BDDs *) 
let set_is_empty b = Mlglu.mdd_is_zero b
let set_and  b1 b2 = Mlglu.mdd_and b1 b2 1 1
let set_or   b1 b2 = Mlglu.mdd_or  b1 b2 1 1
let set_impl b1 b2 = Mlglu.mdd_or  b1 b2 0 1
let set_not  b     = Mlglu.mdd_not b

(** Random simulates a module, given its initial state as a ticc
    expression for the given number of cycles. *)
let simulate (sm: Symmod.t) (expr: string) (nCycles: int)
    (outFile: string) : unit =
  Simulate.simulate sm expr nCycles outFile
;;

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
  Mlglu.mdd_print mgr x

(** prints the list of known modules (just the names) *)
let print_list_modules () = 
  Prog.list_modules_top (); 
  print_string "\n"

(** prints the list of known modules (just the names) *)
let print_list_symmodules () = 
  Symprog.list_modules_top (); 
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

