(** Definition of an explicit (as opposed to symbolic) module. *)

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
  (** initial condition of the module *)
  mutable init  : Ast.t list; 
  (** statesets of the module *)
  mutable ssets : (string, Ast.t) Hsetmap.t; 
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
  init     = []; 
  ssets    = Hsetmap.mk (); 
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
let get_init m = m.init 
let get_ssets m = m.ssets 
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
let iter_ssets  (m: t) (f: string -> Ast.t -> unit) : unit = 
  Hsetmap.iter f m.ssets

(** Checks existence in hash tables *)
let is_lvar_name_def m n = Hsetmap.mem m.lvars n
let is_fvar_name_def m n = Hsetmap.mem m.lvars n
let is_var_name_def m n = Hsetmap.mem m.vars n
let is_sset_name_def m n = Hsetmap.mem m.ssets n 

(** Adds a variable to a module (generic) *)
let add_somevar vhash (v: Var.t) = Hsetmap.add vhash (Var.get_name v) v

(** Adds a local variable to a module *)
let add_lvar m (v: Var.t) = 
  add_somevar m.lvars v; 
  (* a local variable is also a global one *) 
  add_somevar m.vars  v 

(** Adds variables to a module *)
let add_fvar m (v: Var.t) = add_somevar m.fvars v
let add_hvar m (v: Var.t) = add_somevar m.hvars v
let add_var m (v: Var.t)  = add_somevar m.vars  v

(** Adds invariants and initial conditions *)
let add_iinv m e = m.iinv <- e::m.iinv
let add_oinv m e = m.oinv <- e::m.oinv
let add_init m e = m.init <- e::m.init

(** Adds a stateset to a module *)
let add_sset (m : t) (n: string) (e: Ast.t) = Hsetmap.add m.ssets n e

(** Add rules to a module *)
let add_lrule m (r: Rule.lrule_t) : unit = Hsetmap.add m.lrules (Rule.get_lname r) r
let add_orule m (r: Rule.orule_t) : unit = Hsetmap.add m.orules (Rule.get_oname r) r
let add_irule m (r: Rule.irule_t) : unit = Hsetmap.add m.irules (Rule.get_iname r) r

(** Sets invariants and initial conditions *)
let set_iinv m el = m.iinv <- el 
let set_oinv m el = m.oinv <- el 
let set_init m el = m.init <- el 
let set_ssets m el = m.ssets <- el 

(** Looks up variables *) 
let lookup_lvar m n = Hsetmap.find m.lvars n
let lookup_fvar m n = Hsetmap.find m.fvars n
let lookup_var m n = Hsetmap.find m.vars n

(** Looks up statesets *) 
let lookup_sset (m: t) (n: string) : Ast.t = Hsetmap.find m.ssets n 
    
(** Gets variable hashtables *) 
let get_lvars (m : t) = m.lvars
let get_fvars (m : t) = m.fvars
let get_vars (m : t) = m.vars

(** Checks whether names are defined *)
let is_irule_name_def m n = Hsetmap.mem m.irules n
let is_orule_name_def m n = Hsetmap.mem m.orules n
let is_lrule_name_def m n = Hsetmap.mem m.lrules n

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
  let print_init e = 
    Printf.printf "\n  Initial condition: "; 
    Ast.print e;
    Printf.printf ";"
  in List.iter print_init m.init; 
  let print_sset (n: string) (e: Ast.t) : unit = 
    Printf.printf "\n  Stateset %s: " n; 
    Ast.print e;
    Printf.printf ";"
  in iter_ssets m print_sset; 
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
  let print_init e = 
    Printf.printf "\n  Initial condition: "; 
    Ast.print e;
    Printf.printf ";"
  in List.iter print_init m.init; 
  let print_sset (n: string) (e: Ast.t) : unit = 
    Printf.printf "\n  Stateset %s: " n; 
    Ast.print e;
    Printf.printf ";"
  in iter_ssets m print_sset; 
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

