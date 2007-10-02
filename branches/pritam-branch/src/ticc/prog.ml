(** Definition of program (i.e., set of explicit modules). *)

(** This declares the explicit top level. *) 

type t = {
  (** name *)
  name  : string;
  (** set of global variables *)
  vars  : (string, Var.t) Hsetmap.t;
  (** set of modules *)
  mods  : (string, Mod.t) Hsetmap.t; 
  (** set of statesets *)
  ssets : (string, Ast.t) Hsetmap.t;
}

exception Prog_duplicate_var
exception Prog_duplicate_act
exception Prog_duplicate_mod
exception Prog_nomod

(** Create an empty program *)
let mk (name: string) : t =
  {
    name  = name;
    vars  = Hsetmap.mk (); 
    mods  = Hsetmap.mk ();
    ssets = Hsetmap.mk ();
  }
    
(** Erases all the content of a program *)
let cleanup p = 
  Hsetmap.erase p.vars; 
  Hsetmap.erase p.mods;
  Hsetmap.erase p.ssets

(** This is the toplevel program, where all declarations go. 
    For the moment, there is only this program. *)
let toplevel = mk "Toplevel"

(** This is the parselevel, used for parsing, so that if there is a
    parsing error we do not corrupt the toplevel. *)
let parselevel = mk "Parselevel"

(** Folds all declarations into parselevel to top, and cleans up top. *)
let fold_to_top () = 
  Hsetmap.add_to_first toplevel.vars  parselevel.vars;
  Hsetmap.add_to_first toplevel.mods  parselevel.mods;
  Hsetmap.add_to_first toplevel.ssets parselevel.ssets;
  cleanup parselevel 

(** ********** Variables ********************************** *)


(** Gets the name *) 
let get_name p = p.name 

(** Checks whether a variable name is already used *) 
let is_var_name_def p n = Hsetmap.mem p.vars n

let is_var_name_def_top = is_var_name_def toplevel 

(** Adds a variable *)
let add_var p v = Hsetmap.add p.vars (Var.get_name v) v

let add_var_top   = add_var toplevel 

(** Looks up a variable by name *)
let lookup_var p n : Var.t = Hsetmap.find p.vars n

let lookup_var_top = lookup_var toplevel 

(** Return the global variables *)
let get_vars p = p.vars 

let get_vars_top () = get_vars toplevel 

(** Prints the global variables *) 
let print_vars p : unit = 
  Hsetmap.iter_body Var.print p.vars

let print_vars_top () = print_vars toplevel


(** ********** Modules ********************************** *)

(** Checks whether a module name is already defined *)
let is_mod_name_def p n = Hsetmap.mem p.mods n

let is_mod_name_def_top = is_mod_name_def toplevel 

(** Adds a module *)
let add_mod p m = Hsetmap.add p.mods (Mod.get_name m) m

let add_mod_top = add_mod toplevel 

(** Return the modules *)
let get_mods p = p.mods

let get_mods_top () = get_mods toplevel

(** Get a module by name *)
let get_mod p mod_name =
  Hsetmap.find p.mods mod_name

let get_mod_top =
  get_mod toplevel

(** Folds over modules *)
let fold_mods p f a =
  let g x y z = f y z in
  Hsetmap.fold g p.mods a

(** Lists the modules *)
let list_modules p : unit =
  let pr_mod_name m = 
    print_string "\n";
    print_string (Mod.get_name m)
  in
  Hsetmap.iter_body pr_mod_name p.mods

let list_modules_top () = list_modules toplevel 

(** ********** Statesets ********************************** *)

(** Checks whether a stateset name is already defined *)
let is_sset_name_def p n = Hsetmap.mem p.ssets n

let is_sset_name_def_top = is_sset_name_def toplevel 

(** Adds a stateset *)
let add_sset p name exp = Hsetmap.add p.ssets name exp

let add_sset_top = add_sset toplevel 

(** Get a stateset by name *)
let get_sset p name = Hsetmap.find p.ssets name

let get_sset_top = get_sset toplevel

(** ********** Program ********************************** *)

(** Prints the program *)
let print p : unit =
  (* Printf.printf "\n// %s\n" p.name; *)
  Hsetmap.iter_body Var.print p.vars;
  Hsetmap.iter_body Mod.print p.mods

let print_debug p : unit =
  Hsetmap.iter_body Var.print p.vars;
  Hsetmap.iter_body Mod.print_debug p.mods

let print_top () = print toplevel 

let print_debug_top () = print_debug toplevel 

(* **************************************************************** *)

(** These functions are used by the clone_mod function *)

(** get_newname: Rename a variable or a local rule

    This function takes the name [mname] of a module and a name
    [name].  The function replaces the first part of [name] (the part
    before the first ".")  by [mname], and return the new name.
 *)

let get_newname (mname: string) (name: string) : string =
  let vlen = String.length name in
  let pos = String.index name '.' in
  let temp = String.sub name (pos) (vlen - pos) in
  let new_name = mname ^ temp in
  new_name


(** dup_var Create a new variable. This function takes a
    variables [v] and a module name [mname]. The variable [v] is
    supposed to be local.  The function creates a new variable whose
    type is the same as the one of [v].  The name of the new variable
    is identical to the one of v except that the first part of the
    name of [v] becomes [mname] (i.e. *1.*2 -> [mname].*2 ).
 *)
let dup_var mname v : Var.t =
  let vname = Var.get_name v in
  let new_name = get_newname mname vname in
  let new_var = Var.mk_same_type v new_name (Some mname) in
  new_var


(** lookup_renamed_var looks up a variable. 

    This function takes a module [m] and a variable [v]. The functions
    first cheks if the variable [v] is global. In that case It simply
    returns it.  Otherwise, it returns the result of looking up in [m]
    a variable with the name of [m], followed by the name of [v].  *)

let lookup_renamed_var m v : Var.t =
  let mname = Mod.get_name m in
  let vname = Var.get_name v in
  if is_var_name_def_top vname
  then v
  else
    let n = get_newname mname vname in
    Mod.lookup_lvar m n

(** Clone_gcl: cloning of a list of guarded commands. 

    This functions takes a list [gcl] of pairs of Ast and output a new
    list in which the name of the local variables that are referenced
    in each member of [gcl] have been replaced by their new names in
    [m].  *)

let rec clone_gcl m gcl : (Ast.t * Ast.t) list =
  match gcl with
    (g, c) :: tail -> (Ast.replace_vars g (lookup_renamed_var m ),
    Ast.replace_vars c (lookup_renamed_var m ))
      :: clone_gcl m tail
  | [] -> []


(** Clone a command

    This function is the same as the previous one except
    that it works on a list of Ast (not on a list of PAIRs). 
 *)
let rec clone_command cl m : Ast.t list =
  match cl with
    (c) :: tail -> (Ast.replace_vars c (lookup_renamed_var m)) :: clone_command tail m
  | [] -> []

(** Clone a deterministic garded command

    This function is used to clone a list [ilgcl] whose elements are of
    the type (Ast.t, [Ast.t, Ast.t, Ast.t, ..., Ast.t]).  The new list is
    a copy of [ilgcl] except that the local variables have been replaced
    by their clones in [m].

    We have a double recusion:

    1. on each member of [ilgcl]
    2. in the right part of the pair of each member of [ilgcl].

 *)

let rec clone_ligcl m ilgcl : (Ast.t * (Ast.t list)) list =
    match ilgcl with
      (g, c :: ctail) :: tail -> ((Ast.replace_vars g (lookup_renamed_var m),
      (Ast.replace_vars c (lookup_renamed_var m )) ::
        clone_command ctail m)) :: clone_ligcl m tail
    | (g, []) :: tail -> ((Ast.replace_vars g (lookup_renamed_var m), [])) ::
        clone_ligcl m tail
    | [] -> []
	

(** New local rule. 
    This function takes a local rule [rule] and a module [m] as inputs.
    The goal of the function is to clone the local rule. The name of the
    new rule is given by the concatenation of the name of [m] with the
    second part of the name of [rule].
    The local variables of [rule] are replaced by their names in
    [m]. The "body" of the rule is cloned using the same approach.
    The module [m] is the clone.
 *)

let new_lrule m rule : unit =
  let mname = Mod.get_name m in
  let rname = Rule.get_lact rule in
  let lact = get_newname mname rname in
  let lgc_list = Rule.get_lgc_list rule in
  let lrule = Rule.mk_lrule lact lgc_list in
  let lrvars = Rule.get_lrvars rule in
  let lwvars = Rule.get_lwvars rule in
  (* fixing the name of the variables in the rule:
     If global: do not change
     If local : lookup for the new name in [m].
   *)
  let new_lrvars v = Rule.add_lr_var lrule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_lrvars lrvars;
  let new_lwvars v = Rule.add_lw_var lrule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_lwvars lwvars;
  let new_lgc_list = clone_gcl m lgc_list in
  Rule.set_lgc_list lrule new_lgc_list;
  Mod.add_lrule m lrule

(** New output rule.  Same as new_lrule except that the name of the new
    rule is the same as the one of [rule].
 *)

let new_orule m rule : unit =
  let oact = Rule.get_oact rule in
  let ogc_list = Rule.get_ogc_list rule in
  let orule = Rule.mk_orule oact ogc_list in
  let orvars = Rule.get_orvars rule in
  let owvars = Rule.get_owvars rule in
  let new_orvars v = Rule.add_or_var orule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_orvars orvars;
  let new_owvars v = Rule.add_ow_var orule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_owvars owvars;
  let new_ogc_list = clone_gcl m ogc_list in
  Rule.set_ogc_list orule new_ogc_list;
  Mod.add_orule m orule

(** Rename variables in a input rule.
    This function clones an input
    rule [rule]. The principle is identical 
    to the one used in mk_orule and mk_lrule.
 *)

let new_irule m rule : unit =
  let iact = Rule.get_iact rule in
  let gigc_list = Rule.get_global_igc_list rule in
  let ligc_list = Rule.get_local_igc_list rule in
  let irule = Rule.mk_irule iact gigc_list ligc_list in
  let irvars = Rule.get_irvars rule in
  let iwgvars = Rule.get_iwgvars rule in
  let iwlvars = Rule.get_iwlvars rule in
  let new_irvars v = Rule.add_ir_var irule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_irvars irvars;
  let new_iwgvars v = Rule.add_iwg_var irule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_iwgvars iwgvars;
  let new_iwlvars v = Rule.add_iwl_var irule (lookup_renamed_var m v) in
  Hsetmap.iter_body new_iwlvars iwlvars;
  let new_gigc_list = clone_gcl m gigc_list in
  Rule.set_global_igc_list irule new_gigc_list;
  let new_ligc_list = clone_ligcl m ligc_list in
  Rule.set_local_igc_list irule new_ligc_list;
  Mod.add_irule m irule

(** Clones a set of states.
    [m] is the new module; 
    [old_name] is the old name; 
    [expr] is the expression. 
    The function changes the name [old_name] to the name of [m],
    followed by remaining part of the name [old_name] after the first
    ".".  Then, it renames the variables in [expr] appropriately, and 
    adds the stateset to [m].
 *)
let new_sset (m: Mod.t) (old_name: string) (expr: Ast.t) : unit = 
  let new_name = get_newname (Mod.get_name m) old_name in 
  let new_expr = Ast.replace_vars expr (lookup_renamed_var m) in 
  Mod.add_sset m new_name new_expr 

(** Clones a module. 

    This function clones a module whose name is [m1] into a module whose
    name is [m2] in a program [p]. The name [m2] cannot reference an
    existing module in [P]. The cloning is done in the following way.

    1) The local variables are cloned and their name are changed in
    the following way: [m1].* -> [m2].*.

    2) The global variables are not cloned.


    3) New rules are created for the clone [m2].  For each rule of [m1],
    clone_mod creates a rule whose name is identical to the one of the
    rule of [m1] except if the rule is local (in that case, we use
    [m1].* -> [m2].* ). The variables referenced in the rules are
    either cloned or stay the same depending if they are local or
    global.
    
 *)

let clone_modp p m1 m2 : unit =
  if (is_mod_name_def_top m2)
  then begin
    Printf.printf "Error: module %s is already defined \n" m2;
    raise Prog_duplicate_mod
  end 
  else if not (is_mod_name_def_top m1)
  then begin
    Printf.printf "Error: module %s is not defined \n" m1;
    raise Prog_nomod
  end
  else begin
    let mod1 = get_mod p m1 in
    let fvars1 = Mod.get_fvars mod1 in
    let hvars1 = Mod.get_hvars mod1 in
    let init1 = Mod.get_init mod1 in 
    let iinv1 = Mod.get_iinv mod1 in
    let oinv1 = Mod.get_oinv mod1 in
    let mod2 = Mod.create_empty m2 in
    add_mod p mod2;
    let add_lvar v = Mod.add_lvar mod2 (dup_var m2 v) in
    Mod.iter_lvars mod1 add_lvar;
    Hsetmap.iter_body (Mod.add_fvar mod2) fvars1;
    let add_hvar v = Mod.add_hvar mod2 (lookup_renamed_var mod2 v) in
    Hsetmap.iter_body add_hvar hvars1;
    Mod.iter_lvars mod2 (Mod.add_var mod2);
    Hsetmap.iter_body (Mod.add_var mod2) fvars1;
    let hvars2 = Mod.get_hvars mod2 in
    Hsetmap.iter_body (Mod.add_var mod2) hvars2;
    let add_iinv iinv = Mod.add_iinv mod2 (Ast.replace_vars iinv (lookup_renamed_var mod2 )) in
    List.iter add_iinv iinv1;
    let add_oinv oinv = Mod.add_oinv mod2 (Ast.replace_vars oinv (lookup_renamed_var mod2 )) in
    List.iter add_oinv oinv1;
    let add_init init = Mod.add_init mod2 (Ast.replace_vars init (lookup_renamed_var mod2 )) in
    List.iter add_init init1;
    Mod.iter_lrules mod1 (new_lrule mod2);
    Mod.iter_orules mod1 (new_orule mod2);
    Mod.iter_irules mod1 (new_irule mod2);
    Mod.iter_ssets  mod1 (new_sset mod2)
  end

(** End of the clone functions *)

