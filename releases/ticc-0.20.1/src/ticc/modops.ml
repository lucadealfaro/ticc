(** This module contains operations on modules as a whole *)

type stateset_t = Mlglu.mdd;;
module VarSet = Vset.VS;;
type varid_t = Vset.varid_t;; 

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
  let mgr = Symprog.get_mgr Symprog.toplevel in
  let m = 
    try Prog.get_mod_top mod_name
    with Not_found ->
      Printf.printf "\nError: Unknown module %s!\n" mod_name;
      raise Not_found
  in
  (* builds the symbolic module *)
  let sm = Symbuild.mk_mod m Symprog.toplevel in
  (* strengthen invariants to put module into normal form *)
  let strong_iinv = Ops.win_i_safe  Symprog.toplevel sm (Symmod.get_iinv sm)
  and strong_oinv = Ops.win_lo_safe  Symprog.toplevel sm (Symmod.get_oinv sm) in
  Symmod.set_iinv sm strong_iinv;
  Symmod.set_oinv sm strong_oinv;
  let new_init = Mlglu.mdd_and (Mlglu.mdd_and (Symmod.get_init sm)
    strong_iinv 1 1) strong_oinv 1 1 in 
  Symmod.set_init sm new_init; 
  if Mlglu.mdd_is_zero new_init then begin
    Printf.printf "\nThe module %s has an empty set of initial states!\n" mod_name; 
    flush stdout
  end; 
  (** the result *)
  sm


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

(* **************************************************************** *)
(*                                                                  *)
(*  Forgets a module                                                *)
(*                                                                  *)
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
  Symmod.clear_lrules sm; 
  Symmod.clear_irules sm; 
  Symmod.clear_orules sm
;;




