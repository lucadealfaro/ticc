(** $Id: symmod.ml,v 1.24 2005/10/18 02:47:11 luca Exp $ *)

(** Container for symbolic module *) 

exception WrongRuleType
exception UnknownClock 

module VarSet = Vset.VS

type varid_t = Vset.varid_t 

type rule_type_t = Local | Input | Output

type rule_body_t = 
    (** transition relation for local rules *)
    Loc of Mlglu.mdd 
    (** transition relation for output rules *)
  | Out of Mlglu.mdd 
    (** transition relation for input rules: (global, local) *)
  | Inp of Mlglu.mdd * Mlglu.mdd


type rule_t = {
  act:   string;
  (** list of all primed variables.  This is used to 
    * implement defaults.   In the case of an Input rule,
    * 'wvars' refers to the primed variables of the InputLocal rule. *) 
  wvars: VarSet.t;
  (** mdd representing the rule, two mdds in case of input rules 
      In the case of an input rule, in the global part, 
      the variables that are primed must be global.
      In the local part, the variables that are primed must be local.
      TO CHECK: are the previous comments checked during the parsing?
      *)

  rule:  rule_body_t;
}

type t = {
  name: string;
  (** list of all mentioned variables *) 
  mutable vars  : VarSet.t;
  (** list of local variables *)
  mutable lvars : VarSet.t; 
  (** list of global variables *) 
  mutable gvars : VarSet.t; 
  (** list of history-ful variables *)
  mutable hvars : VarSet.t; 
  (** clock variables *)
  mutable cvars : VarSet.t; 
  (** bounds of clock variables *) 
  clock_bound : (varid_t, int) Hsetmap.t; 
  (** input invariants *) 
  mutable iinv   : Mlglu.mdd; 
  (** output invariants *)
  mutable oinv   : Mlglu.mdd; 
  (** local output transition rules *) 
  lrules : (string, rule_t) Hsetmap.t; 
  (** input transition rules, global and local *) 
  irules : (string, rule_t) Hsetmap.t; 
  (** output transition rules *) 
  orules : (string, rule_t) Hsetmap.t; 
}

(** Makes an empty symbolic module. 
    We cannot even put in the variable IDs, since those are in 
    symprog. *)

let mk mgr (name: string) : t =
  {
    name = name;

    vars  = VarSet.empty; 
    lvars = VarSet.empty; 
    gvars = VarSet.empty; 
    hvars = VarSet.empty; 
    cvars = VarSet.empty; 

    clock_bound = Hsetmap.mk (); 

    iinv = Mlglu.mdd_one mgr;
    oinv = Mlglu.mdd_one mgr;

    lrules = Hsetmap.mk ();
    irules = Hsetmap.mk ();
    orules = Hsetmap.mk ();
  }


(** Access functions *)
let add_var  (m: t) (id: varid_t) = m.vars  <- VarSet.add id m.vars 
let add_lvar (m: t) (id: varid_t) = m.lvars <- VarSet.add id m.lvars 
let add_gvar (m: t) (id: varid_t) = m.gvars <- VarSet.add id m.gvars 
let add_hvar (m: t) (id: varid_t) = m.hvars <- VarSet.add id m.hvars 
let add_cvar (m: t) (id: varid_t) (bound: int) = 
  m.cvars <- VarSet.add id m.cvars; 
  Hsetmap.add m.clock_bound id bound 

let set_iinv m phi = m.iinv <- phi
let set_oinv m phi = m.oinv <- phi
let get_iinv m = m.iinv
let get_oinv m = m.oinv


let get_name (m: t) : string   = m.name
let get_vars (m: t) : VarSet.t = m.vars
let get_lvars (m: t) : VarSet.t = m.lvars
let get_gvars (m: t) : VarSet.t = m.gvars
let get_hvars (m: t) : VarSet.t = m.hvars
let get_cvars (m: t) : VarSet.t = m.cvars

let get_clock_bound (m: t) (id: varid_t) : int = 
  if not (VarSet.mem id m.cvars) then raise UnknownClock 
  else Hsetmap.find m.clock_bound id 


let get_rules (m: t) act : rule_t list =
    List.fold_left
	(fun found_rules rule_collection ->
	    (* check the next collection, and prepend its rule
	     * to the front of our list of found rules. *)
	    try (Hsetmap.find rule_collection act)::found_rules
	    with Not_found -> found_rules
	)
	[]
	[ m.lrules; m.irules; m.orules ]
    ;;

let get_irule (m: t) act : rule_t option = 
    try Some (Hsetmap.find m.irules act)
    with Not_found -> None
    ;;

let has_action (m: t) act : bool =
    let rule_list = get_rules m act in
    (List.length rule_list > 0)
    ;;


(** A module is timed if it has at least one clock *) 
let is_timed (m: t) : bool = not (VarSet.is_empty m.cvars) 

(** **************** Rules ******************* *)

(** Internal function to make a rule of any type. *)
let mk_rule typ act wvars tran : rule_t =
  let rule = match typ with
      Local  -> Loc (List.hd tran)
    | Output -> Out (List.hd tran)
    | Input -> Inp ((List.nth tran 0), (List.nth tran 1))
		(* TODO: why doesn't the last line use hd & tl? *)
  in 
  {
    act = act;
    wvars = wvars;
    rule = rule;
  }
  
let mk_lrule act wvars tran : rule_t = 
  mk_rule Local  act wvars [tran]

let mk_orule act wvars tran : rule_t = 
  mk_rule Output act wvars [tran]

let mk_irule act wvars trang tranl : rule_t = 
  mk_rule Input  act wvars [trang; tranl]

(** Returns the type of a rule. *)
let get_rule_type r : rule_type_t =
  match r.rule with
    Loc _ -> Local
  | Inp _ -> Input
  | Out _ -> Output

(** Returns the action of a rule. *)
let get_rule_act  r : string   = r.act

(** Returns the set of variables that may be written by [r].

  Precisely:
  - For output/local rules, 
  it's the set of variables (both global and local) 
  that occur primed in any command of [r].

  - For input rules, it returns the set of _local_ variables
  that occur primed in any command of the _local_ portion. 
*)
let get_rule_wvars r : VarSet.t = r.wvars

(** Returns the transition relation of the rule. 
    It is better to return r.rule rather than a list of mdds. 
    The list anyway is dishomogeneous, and matching it is a mess. 
    Instead, this way one can match the outcome of get_rule_tran, 
    and avoid both ambiguity, and a separate call to 
    get_rule_type. *) 
let get_rule_tran r = r.rule 

(** Returns the transition relations as a list.
    This way, you can extract the MDDs if you already know the type. *)
let get_rule_tran_as_pair r =
    match get_rule_tran r with
	Loc (mdd)		-> (mdd, mdd)
      | Out (mdd)		-> (mdd, mdd)
      | Inp (mdd1, mdd2)	-> (mdd1, mdd2)
;;

(** Returns the local and global MDDs for an input transition relation *) 
let get_rule_ig_il_mdds r = 
    match get_rule_tran r with
      Loc (mdd)	-> raise WrongRuleType
    | Out (mdd)	-> raise WrongRuleType
    | Inp (mdd1, mdd2) -> (mdd1, mdd2)
;;

(** Returns the MDD for an output rule *) 
let get_orule_mdds r = 
    match get_rule_tran r with
	Loc (mdd)	-> raise WrongRuleType
      | Out (mdd)	-> mdd 
      | Inp (mdd1, mdd2) -> raise WrongRuleType
;;
    

(** Adds a rule to a module *) 
let add_rule m r = 
    let act = get_rule_act r in
    match get_rule_type r with
	Local  -> Hsetmap.add m.lrules act r
      | Input  -> Hsetmap.add m.irules act r
      | Output -> Hsetmap.add m.orules act r

(** Iterators *)
let iter_lrules m f = Hsetmap.iter_body f m.lrules
let iter_irules m f = Hsetmap.iter_body f m.irules
let iter_orules m f = Hsetmap.iter_body f m.orules

