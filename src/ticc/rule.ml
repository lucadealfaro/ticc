(** $Id: rule.ml,v 1.25 2005/09/19 23:26:41 luca Exp $ *)

(** Type of a local transition rule *)
type lrule_t = {
  (** name *)
  lact   : string; 
  (** read variables *)
  lrvars  : (string, Var.t) Hsetmap.t;
  (** written variables *)
  lwvars  : (string, Var.t) Hsetmap.t;
  (** guard, command *)
  mutable lgc_list : (Ast.t * Ast.t) list; 
}

(** Type of an output rule for a global action *)
type orule_t = {
  (** action *)
  oact   : string;
  (** read variables *)
  orvars  : (string, Var.t) Hsetmap.t;
  (** written variables *)
  owvars  : (string, Var.t) Hsetmap.t;
  (** List of guarded commands, in the form (guard, command) *)
  mutable ogc_list : (Ast.t * Ast.t) list; 
}

(** Type of an input rule for a global action *)
type irule_t = {
  (** name *)
  iact   : string;
  (** read variables, by the local and global parts.
      This includes the primed global variables that are used in the local part. *)
  irvars  : (string, Var.t) Hsetmap.t;
  (** written variables by the global part *)
  iwgvars  : (string, Var.t) Hsetmap.t; 
  (** written variables by the local part *)
  iwlvars  : (string, Var.t) Hsetmap.t; 
  (** guard, command, for the global variables *)
  mutable global_igc_list : (Ast.t * Ast.t) list; 
  (** For the local variables, it is deterministic. 
      So it in the format of a list of "deterministic guarded 
      commands" (dgc), where each dgc is of the form: 
      (guard, [e1, e2, e3, ..., en]), and each ei has the 
      form x' = expr for some primed local variable x'. *)
  mutable local_igc_list : (Ast.t * (Ast.t list)) list; 
}

let mk_lrule a g : lrule_t = {
  lact     = a; 
  lrvars   = Hsetmap.mk (); 
  lwvars   = Hsetmap.mk (); 
  lgc_list = g
}

let mk_orule a g : orule_t = {
  oact     = a; 
  orvars   = Hsetmap.mk (); 
  owvars   = Hsetmap.mk (); 
  ogc_list = g 
}

let mk_irule a g l : irule_t = {
  iact    = a; 
  irvars  = Hsetmap.mk (); 
  iwgvars = Hsetmap.mk (); 
  iwlvars = Hsetmap.mk (); 
  global_igc_list = g; 
  local_igc_list = l
}

let get_lname (r : lrule_t) = r.lact
let get_oname (r : orule_t) = r.oact
let get_iname (r : irule_t) = r.iact

let get_lact (r : lrule_t) = r.lact
let get_oact (r : orule_t) = r.oact
let get_iact (r : irule_t) = r.iact

let get_orvars (r : orule_t) = r.orvars
let get_owvars (r : orule_t) = r.owvars

let get_irvars (r : irule_t) = r.irvars
let get_iwlvars (r : irule_t) = r.iwlvars
let get_iwgvars (r : irule_t) = r.iwgvars

let get_lrvars (r : lrule_t) = r.lrvars
let get_lwvars (r : lrule_t) = r.lwvars

let get_ogc_list (r : orule_t) = r.ogc_list
let get_lgc_list (r : lrule_t) = r.lgc_list 
let get_global_igc_list (r : irule_t) = r.global_igc_list
let get_local_igc_list  (r : irule_t) = r.local_igc_list

let set_ogc_list (r : orule_t) l : unit = r.ogc_list <- l
let set_lgc_list (r : lrule_t) l : unit = r.lgc_list <- l
let set_global_igc_list (r : irule_t) l : unit = r.global_igc_list <- l
let set_local_igc_list  (r : irule_t) l : unit = r.local_igc_list  <- l


(** Adds variable to read vars of rule, if not already there. *)
let add_or_var (r: orule_t) (v : Var.t) = 
  Hsetmap.add r.orvars (Var.get_name v) v 
let add_lr_var (r: lrule_t) (v : Var.t) = 
  Hsetmap.add r.lrvars (Var.get_name v) v 
let add_ir_var (r: irule_t) (v : Var.t) = 
  Hsetmap.add r.irvars (Var.get_name v) v 

(** Adds variable to written vars of rule, if not already there. *)
let add_ow_var (r: orule_t) (v : Var.t) = 
  Hsetmap.add r.owvars (Var.get_name v) v 
let add_lw_var (r: lrule_t) (v : Var.t) = 
  Hsetmap.add r.lwvars (Var.get_name v) v 
let add_iwg_var (r: irule_t) (v : Var.t) = 
  Hsetmap.add r.iwgvars (Var.get_name v) v 
let add_iwl_var (r: irule_t) (v : Var.t) = 
  Hsetmap.add r.iwlvars (Var.get_name v) v 

(** prints a local rule *)
let print_lrule (r: lrule_t) = 
  Printf.printf "\n  local %s: " r.lact ;
  let f (e1, e2) = 
    Printf.printf "\n\t"; 
    Ast.print e1; 
    Printf.printf " ==> "; 
    Ast.print e2;
    Printf.printf ";"
  in List.iter f r.lgc_list;
  Printf.printf "\n    endlocal"

(** Prints an output transition rule *)
let print_orule (r: orule_t) = 
  Printf.printf "\n  output %s: " r.oact ;

  let f (e1, e2) = 
    Printf.printf "\n\t"; 
    Ast.print e1; 
    Printf.printf " ==> "; 
    Ast.print e2;
    Printf.printf ";"
  in List.iter f r.ogc_list;
  Printf.printf "\n    endoutput"


(** Prints an output transition rule *)
let print_debug_orule (r: orule_t) = 
  Printf.printf "\n  output %s: " r.oact ;

  Printf.printf "\n Vars ";
  Hsetmap.iter_body Var.print r.orvars;
  Printf.printf " -> ";
  Hsetmap.iter_body Var.print r.owvars;
  Printf.printf "\n";

  let f (e1, e2) = 
    Printf.printf "\n\t"; 
    Ast.print e1; 
    Printf.printf " ==> "; 
    Ast.print e2;
    Printf.printf ";"
  in List.iter f r.ogc_list;
  Printf.printf "\n    endoutput"


(** Prints an input transition rule *)
let print_irule (r: irule_t) = 
  Printf.printf "\n  input %s: " r.iact ;

  Printf.printf "\n    global:"; 
  let f (e1, e2) =
    Printf.printf "\n\t"; 
    Ast.print e1; 
    Printf.printf " ==> "; 
    Ast.print e2;
    Printf.printf ";"
  in List.iter f r.global_igc_list; 
  Printf.printf "\n    local:";
  let g (e, fl) = 
    let rec h = function 
	[] -> ()
      | [asg] -> Ast.print_asg asg
      | asg1::asg2::asgl -> 
	  Ast.print_asg asg1;
	  Printf.printf ", "; 
	  h (asg2::asgl)
    in 
    Printf.printf "\n\t"; 
    Ast.print e; 
    Printf.printf " ==> "; 
    h fl;
    Printf.printf ";"
  in List.iter g r.local_igc_list;
  Printf.printf "\n    endinput"

