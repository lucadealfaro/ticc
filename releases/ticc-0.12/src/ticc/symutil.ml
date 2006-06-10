(** $Id: symutil.ml,v 1.5 2005/09/19 23:26:41 luca Exp $ *)

(** This file contains some useful functions for working on MDDs. *) 

module VarSet = Vset.VS

type varid_t = Vset.varid_t 


(** Prints a set of symbolic variables *)
let print_varset sp (var_set: VarSet.t) : unit =
  Printf.printf "\n{ ";

  let print_one vid =
    let (v, p) = Symprog.get_var_p sp vid in
    Var.print_name v;
    if p then Printf.printf "'";
    Printf.printf " "
  in
  VarSet.iter print_one var_set;
  Printf.printf "}"


(** This function takes an mdd [a]  and a [var_set], and returns 
    [ a[var_set'/var_set] ] 
  [var_set] should only contains unprime variable ids *) 
let prime_mdd_vars symtop a var_set = 
  let mgr = Symprog.get_mgr symtop in 
  let unp_list = Vset.to_list var_set in 
  let f = Symprog.prime_id symtop in 
  let pri_list = List.map f unp_list in 
  Mlglu.mdd_substitute_two_lists mgr a unp_list pri_list 


(** This function takes an mdd [a]  and a [var_set'], and returns 
    [ a[var_set/var_set'] ] 
  [var_set] should only contains prime variable ids *) 
let unprime_mdd_vars symtop a var_set = 
  let mgr = Symprog.get_mgr symtop in 
  let pri_list = Vset.to_list var_set in 
  let f = Symprog.unprime_id symtop in 
  let unp_list = List.map f pri_list in 
  Mlglu.mdd_substitute_two_lists mgr a pri_list unp_list 

(** This function replaces all variables in an mdd [a]
    of a symbolic module [sm] with their primed version. *)
let prime_mdd symtop sm a = 
  let mgr = Symprog.get_mgr symtop in
  let vAll = Symmod.get_vars sm in
  prime_mdd_vars symtop a vAll
 
(** This function replaces all variables in an mdd [a]
    of a symbolic module [sm] with their unprimed version. *)
let unprime_mdd symtop sm a = 
  let mgr = Symprog.get_mgr symtop in
  let vAll = Symmod.get_vars sm in
  unprime_mdd_vars symtop a vAll

 
