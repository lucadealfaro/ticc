(** $Id: mod.ml,v 1.38 2005/09/01 22:12:50 luca Exp $ *)


(* This is the type of a module *) 
type t = {
  (** name of the module *) 
  name	        : string;
  (** list of local variables *) 
  lvars         : (string, Var.t) Hsetmap.t;
  (** list of history-free variables *) 
  fvars         : (string, Var.t) Hsetmap.t;
  (** list of history-full variables *) 
  mutable hvars : (string, Var.t) Hsetmap.t;
  (** list of all mentioned variables *) 
  vars          : (string, Var.t) Hsetmap.t;
  (** input invariants *) 
  mutable iinv  : Ast.t list; 
  (** output invariants *)
  mutable oinv  : Ast.t list; 
  (** local output transition rules *) 
  lrules        : (string, Rule.lrule_t) Hsetmap.t; 
  (** input transition rules, global and local *) 
  irules        : (string, Rule.irule_t) Hsetmap.t; 
  (** output transition rules *) 
  orules        : (string, Rule.orule_t) Hsetmap.t; 
}

(** To signal duplicate declarations *)
exception Mod_duplicate_var
exception Mod_duplicate_input_rule
exception Mod_duplicate_output_rule
exception Mod_duplicate_local_rule

(** Create an empty module without actions and variables *)
let create_empty (name: string) : t =
{
  name     = name;
  lvars    = Hsetmap.mk ();
  fvars    = Hsetmap.mk ();
  hvars    = Hsetmap.mk ();
  vars     = Hsetmap.mk ();
  iinv     = [];
  oinv     = [];
  lrules   = Hsetmap.mk ();
  irules   = Hsetmap.mk ();
  orules   = Hsetmap.mk ();
}

(** Access functions *) 
let get_name  m = m.name 
let get_lvars m = m.lvars
let get_fvars m = m.fvars
let get_hvars m = m.hvars
let get_vars  m = m.vars
let get_iinv  m = m.iinv
let get_oinv  m = m.oinv
let get_lrules m = m.lrules
let get_irules m = m.irules
let get_orules m = m.orules

(** Iterators *) 

let iter_lvars m f  = Hsetmap.iter_body f m.lvars
let iter_vars  m f  = Hsetmap.iter_body f m.vars
let iter_lrules m f = Hsetmap.iter_body f m.lrules
let iter_irules m f = Hsetmap.iter_body f m.irules
let iter_orules m f = Hsetmap.iter_body f m.orules

(** Checks whether a local variable name is already used *) 
let is_lvar_name_def m n = Hsetmap.mem m.lvars n

(** Checks whether a free variable name is already used *) 
let is_fvar_name_def m n = Hsetmap.mem m.lvars n

(** Checks whether a global variable name is already used *) 
let is_var_name_def m n = Hsetmap.mem m.vars n


(** Adds a variable to a module (generic) *)
let add_somevar vhash (v: Var.t) = Hsetmap.add vhash (Var.get_name v) v

(** Adds a local variable to a module *)
let add_lvar m (v: Var.t) = 
  add_somevar m.lvars v; 
  (* a local variable is also a global one *) 
  add_somevar m.vars  v 

(** Adds a free variable to a module *)
let add_fvar m (v: Var.t) = add_somevar m.fvars v

(** Adds a history variable to a module *)
let add_hvar m (v: Var.t) = add_somevar m.hvars v

(** Adds a variable to the list of mentioned variables *)
let add_var m (v: Var.t)  = add_somevar m.vars  v

(** Looks up a single local variable *)
let lookup_lvar m n = Hsetmap.find m.lvars n
(** Looks up a single free variable *)
let lookup_fvar m n = Hsetmap.find m.fvars n
(** Looks up a single global variable *)
let lookup_var m n = Hsetmap.find m.vars n
    
(** Return the local variables *)
let get_lvars (m : t) = m.lvars
(** Return the free variables *)
let get_fvars (m : t) = m.fvars
(** Return the local variables *)
let get_vars (m : t) = m.vars

(** Add an input invariant to a module *)
let add_iinv m e = m.iinv <- e::m.iinv

(** Sets the input invariants of a module *)
let set_iinv m el = m.iinv <- el 

(** Add an output invariant to a module *)
let add_oinv m e = m.oinv <- e::m.oinv

(** Sets the output invariants of a module *)
let set_oinv m el = m.oinv <- el 

(** return the input invariants *)
let get_iinv (m : t) = m.iinv

(** return the output invariants *)
let get_oinv (m : t) = m.oinv

(** Checks whether an input rule name is already used *) 
let is_irule_name_def m n = Hsetmap.mem m.irules n

(** Checks whether an output rule name is already used *) 
let is_orule_name_def m n = Hsetmap.mem m.orules n

(** Checks whether an input rule name is already used *) 
let is_lrule_name_def m n = Hsetmap.mem m.lrules n

(** Add a local rule to a module *)
let add_lrule m (r: Rule.lrule_t) : unit = 
  Hsetmap.add m.lrules (Rule.get_lname r) r

(** Add an output rule to a module *)
let add_orule m (r: Rule.orule_t) : unit = 
  Hsetmap.add m.orules (Rule.get_oname r) r

(** Add an input rule to a module *)
let add_irule m (r: Rule.irule_t) : unit = 
  Hsetmap.add m.irules (Rule.get_iname r) r

(*
(** Remove input rule *)
let remove_irule m r = 
  let n = Rule.get_iname r in 
  Hsetmap.remove m.irules n 

(** Remove output rule *)
let remove_orule m r = 
  let n = Rule.get_oname r in 
  Hsetmap.remove m.orules n 

(** Remove local rule *)
let remove_lrule m r = 
  let n = Rule.get_lname r in 
  Hsetmap.remove m.lrules n 
 *)

(** Fixes the set of history variables in a module *) 
let set_hvars (m: t) : unit = 
  m.hvars <- (Hsetmap.unsafe_difference m.vars m.fvars)


(** Print module *)
let print_debug (m : t) : unit =
  Printf.printf "\n\nmodule %s:" m.name;

  Printf.printf "\n\n Local variables:";
  Hsetmap.iter_body Var.print m.lvars;
  Printf.printf "\n\n History-free variables:";
  Hsetmap.iter_body Var.print m.fvars;
  Printf.printf "\n\n All variables:";
  Hsetmap.iter_body Var.print m.vars;
  Printf.printf "\n";
  let print_ialone e = 
    Printf.printf "\n  iinv: ";
    Ast.print e;
    Printf.printf ";"
  in List.iter print_ialone m.iinv; 
  let print_oalone e = 
    Printf.printf "\n  oinv: ";
    Ast.print e;
    Printf.printf ";"
  in List.iter print_oalone m.oinv; 
  Printf.printf "\n"; 
  Hsetmap.iter_body Rule.print_lrule m.lrules; 
  Hsetmap.iter_body Rule.print_debug_orule m.orules; 
  Hsetmap.iter_body Rule.print_irule m.irules; 
  Printf.printf "\nendmodule"

let print (m : t) : unit =
  Printf.printf "\n\nmodule %s:" m.name;
  Hsetmap.iter_body Var.print_if_local m.vars;
  Printf.printf "\n";
  let print_ialone e = 
    Printf.printf "\n  iinv: ";
    Ast.print e;
    Printf.printf ";"
  in List.iter print_ialone m.iinv; 
  let print_oalone e = 
    Printf.printf "\n  oinv: ";
    Ast.print e;
    Printf.printf ";"
  in List.iter print_oalone m.oinv; 
  Printf.printf "\n"; 
  Hsetmap.iter_body Rule.print_lrule m.lrules; 
  Hsetmap.iter_body Rule.print_orule m.orules; 
  Hsetmap.iter_body Rule.print_irule m.irules; 
  Printf.printf "\nendmodule"

