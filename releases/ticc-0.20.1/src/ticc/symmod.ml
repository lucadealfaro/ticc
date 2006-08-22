(** symmod.ml *)

(** Container for symbolic module *) 

exception WrongRuleType
exception UnknownClock 

type stateset_t = Mlglu.mdd;;

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
    (** Action name *) 
    act:   string;
    (** Name of symbolic module where the rule originated. 
	Useful for ATL. *)
    mod_name: string; 
    (** list of all primed variables.  This is used to 
	implement defaults.   In the case of an Input rule,
	'wvars' refers to the primed variables of the InputLocal
	rule. *) 
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
  mutable clock_bound : (varid_t, int) Hsetmap.t; 
  (** initial condition *)
  mutable init   : Mlglu.mdd; 
  (** Set of reachable states *)
  mutable reachset : Mlglu.mdd option; 
  (** statesets *)
  mutable ssets : (string, Mlglu.mdd) Hsetmap.t; 
  (** input invariants *) 
  mutable iinv   : Mlglu.mdd; 
  (** output invariants *)
  mutable oinv   : Mlglu.mdd; 
  (** local output transition rules *) 
  lrules : (string, rule_t) Hsetmap.t; 
  (** input transition rules, global and local *) 
  irules : (string, rule_t) Hsetmap.t; 
  (** output transition rules *) 
  orules : (string, rule_t list) Hsetmap.t; 
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
    
    init = Mlglu.mdd_one mgr;
    reachset = None; 
    ssets = Hsetmap.mk (); 

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

let set_vars  (m: t) (vs: VarSet.t) = m.vars  <- vs
let set_lvars (m: t) (vs: VarSet.t) = m.lvars <- vs
let set_gvars (m: t) (vs: VarSet.t) = m.gvars <- vs
let set_hvars (m: t) (vs: VarSet.t) = m.hvars <- vs
let set_cvars (m: t) (vs: VarSet.t) = m.cvars <- vs
let set_clk_bounds (m: t) (bounds: (varid_t, int) Hsetmap.t) = m.clock_bound <- bounds
let set_ssets (m: t) (set : (string, Mlglu.mdd) Hsetmap.t) = m.ssets <- set 
let set_iinv m phi = m.iinv <- phi
let set_oinv m phi = m.oinv <- phi
let set_init m phi = m.init <- phi
let set_reachset (m: t) (b: Mlglu.mdd option) : unit = m.reachset <- b
(* these are used mostly to forget a module *)
let clear_ssets  m = Hsetmap.erase m.ssets
let clear_lrules m = Hsetmap.erase m.lrules
let clear_irules m = Hsetmap.erase m.irules
let clear_orules m = Hsetmap.erase m.orules

let get_iinv m = m.iinv
let get_oinv m = m.oinv
let get_init m = m.init 
let get_ssets (m: t) : (string, Mlglu.mdd) Hsetmap.t = m.ssets
let iter_ssets (m: t) (f: string -> Mlglu.mdd -> unit) : unit = 
  Hsetmap.iter f m.ssets

let get_name (m: t) : string   = m.name
let get_vars (m: t) : VarSet.t = m.vars
let get_lvars (m: t) : VarSet.t = m.lvars
let get_gvars (m: t) : VarSet.t = m.gvars
let get_hvars (m: t) : VarSet.t = m.hvars
let get_cvars (m: t) : VarSet.t = m.cvars
let get_bounds (m: t) : (varid_t, int) Hsetmap.t = m.clock_bound 

let get_clock_bound (m: t) (id: varid_t) : int = 
  if not (VarSet.mem id m.cvars) then raise UnknownClock 
  else Hsetmap.find m.clock_bound id 

(** A module is timed if it has at least one clock *) 
let is_timed (m: t) : bool = not (VarSet.is_empty m.cvars) 


(** **************** Rules ******************* *)

(** Iterators *)
let iter_lrules m f = Hsetmap.iter_body f m.lrules
let iter_irules m f = Hsetmap.iter_body f m.irules
let iter_orules m f = 
    let g = List.iter f in 
    Hsetmap.iter_body g m.orules

let fold_lrules m f = Hsetmap.fold_body f m.lrules
let fold_irules m f = Hsetmap.fold_body f m.irules
let fold_orules m f = 
    let g = List.fold_right f in 
    Hsetmap.fold_body g m.orules

(** The function gets a rule, and clones it *)

let rule_dup (r: rule_t) : rule_t = 
    let act = r.act in 
    (* no need to dupe a set, as it is functional *)
    let wvars = r.wvars in 
    let mod_name = r.mod_name in 
    let new_r = 
	match r.rule with
	  Loc (m) | Out (m) -> Loc (Mlglu.mdd_dup m)
	| Inp (m1, m2) -> Inp (Mlglu.mdd_dup m1, Mlglu.mdd_dup m2)
    in 
    { 
	act = act;
	mod_name = mod_name; 
	wvars = wvars; 
	rule = new_r;
    }

(** Checks whether a module has a given action.  The check is done by 
    text equality, disregarding wildcards. *)
let has_action (m: t) act : bool =
    (Hsetmap.mem m.lrules act) || (Hsetmap.mem m.irules act) ||
	(Hsetmap.mem m.orules act)
let has_iaction (m: t) act : bool = Hsetmap.mem m.irules act
let has_oaction (m: t) act : bool = Hsetmap.mem m.lrules act
let has_laction (m: t) act : bool = Hsetmap.mem m.orules act

(** Gets a rule, or list of rules, given the name of the action *)
let get_lrule (m: t) (act: string) : rule_t = Hsetmap.find m.lrules act
let get_orule (m: t) (act: string) : rule_t list = Hsetmap.find m.orules act
let get_irule (m: t) (act: string) : rule_t = Hsetmap.find m.irules act

(** Removes an input rule.  *)
let remove_irule (m: t) (act: string) : unit = Hsetmap.remove m.irules act 

(** Checks whether a module has a given input action, giving either a
    perfect match (if any), or the longest wildcard match. *)
let has_input_action_wild (m: t) act : bool =
    if Hsetmap.mem m.irules act 
    then true 
    else begin
	let len_act = String.length act in 
	let found = ref false in 
	(* This function will be iterated on all input actions of m *)
	let search_match (a: string) (r: rule_t) : unit = 
	    let len_a = String.length a in 
	    if a.[len_a - 1] = '*' && len_a - 1 <= len_act then 
		found := !found || (
		    (Str.first_chars act (len_a - 1)) = (Str.first_chars a (len_a - 1)))
	in Hsetmap.iter search_match m.irules; 
	!found
    end

(** This function looks for an input rule in a module, looking for the
    best possible match with the given name, which can itself be
    a wildcard.  It returns the rule, if found. *)
let best_rule_match (m: t) (act: string) : rule_t option = 
    (* A precise match is the best, if found *)
    if Hsetmap.mem m.irules act 
    then Some (Hsetmap.find m.irules act)
    else begin
	let root_act = 
	    if (Str.last_chars act 1) = "*"
	    then Str.first_chars act ((String.length act) - 1)
	    else act
	in
	let best_match = ref (None) in 
	let match_len  = ref (-1) in 
	let len_act = String.length root_act in 
	(* This function will be iterated on all input actions of m *)
	let search_match (a: string) (r: rule_t) : unit = 
	    let len_a = String.length a in 
	    if a.[len_a - 1] = '*' && len_a - 1 <= len_act then 
		(* If it finds a match, and it is a best match *)
		if (Str.first_chars root_act (len_a - 1)) =
		    (Str.first_chars a (len_a - 1)) 
		    && len_a - 1 > !match_len then begin
			(* Found a match a best match *)
			best_match := Some r;
			match_len := len_a - 1
		    end
        in
        Hsetmap.iter search_match m.irules;
        !best_match
    end

(** Returns the type of a rule. *)
let get_rule_type (r: rule_t) : rule_type_t =
  match r.rule with
    Loc _ -> Local
  | Inp _ -> Input
  | Out _ -> Output

(** Returns the action of a rule. *)
let get_rule_act  r : string   = r.act

(** Returns the module where the action originated *)
let get_owner_module r : string = r.mod_name

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
let get_rule_ig_il_mdds (r: rule_t) : Mlglu.mdd * Mlglu.mdd = 
    match get_rule_tran r with
      Loc (mdd)	-> raise WrongRuleType
    | Out (mdd)	-> raise WrongRuleType
    | Inp (mdd1, mdd2) -> (mdd1, mdd2)
;;

(** Returns the MDD for an output rule *) 
let get_orule_mdd (r: rule_t) : Mlglu.mdd = 
    match get_rule_tran r with
	Loc (mdd)	-> raise WrongRuleType
      | Out (mdd)	-> mdd 
      | Inp (mdd1, mdd2) -> raise WrongRuleType
;;
    
(* ********  Making rules   *)

(** Internal function to make a rule of any type. *)
let mk_rule (typ: rule_type_t) (act: string) (mod_name: string) (wvars:VarSet.t) (tran: Mlglu.mdd list) : rule_t =
    let rule = match typ with
	  Local  -> Loc (List.hd tran)
	| Output -> Out (List.hd tran)
	| Input -> Inp ((List.nth tran 0), (List.nth tran 1))
    in 
    {
	act = act;
	mod_name = mod_name; 
	wvars = wvars;
	rule = rule;
    }

(** [mk_lrule act mn wvars tran] makes a local rule, for action [act], 
    belonging originally to a symbolic module of name [mn], 
    with set of modified variables [wvars], 
    and with transition relation [tran]. *)
let mk_lrule (act: string) (mn: string) (wvars: VarSet.t) (tran: Mlglu.mdd) : rule_t = 
    mk_rule Local  act mn wvars [tran]

(** [mk_orule act mn wvars tran] makes an output rule, for action [act], 
    belonging originally to a symbolic module of name [mn], 
    with set of modified variables [wvars], 
    and with transition relation [tran]. *)
let mk_orule (act: string) (mn: string) (wvars: VarSet.t) (tran: Mlglu.mdd) : rule_t = 
    mk_rule Output act mn wvars [tran]

(** [mk_orule act wvars trang tranl] makes an input rule, for action [act], 
    with set of modified variables [wvars], 
    with global transition relation [gtran], 
    and with local transition relation [ltran]. *)
let mk_irule (act: string) (wvars: VarSet.t) (trang: Mlglu.mdd) (tranl: Mlglu.mdd) : rule_t = 
    (* note that the module name is not important for input rules *)
    mk_rule Input  act "" wvars [trang; tranl]

(** Adds a rule to a module *) 
let add_rule (m : t) (r : rule_t) : unit = 
    let act = get_rule_act r in
    match get_rule_type r with
	Local  -> Hsetmap.add m.lrules act r
      | Input  -> Hsetmap.add m.irules act r
      | Output -> 
	    if Hsetmap.mem m.orules act then begin
		let l = Hsetmap.find m.orules act in 
		Hsetmap.modify m.orules act (r :: l)
	    end 
	    else Hsetmap.add m.orules act [r]


(** **************************************************************** *)

(** The function takes a symbolic module and returns a clone. *)

let symbolic_clone mgr (m : t) : t = 
  let m1 = mk mgr m.name in 
  (* We don't need to clone sets of variables, as they are functional *)
  m1.vars  <- m.vars;
  m1.lvars <- m.lvars;
  m1.gvars <- m.gvars;
  m1.hvars <- m.hvars;
  m1.cvars <- m.cvars;
  (* The bounds for the clock variables... They should not change, but 
     just for safety, let's not skimp *)
  m1.clock_bound <- Hsetmap.copy m.clock_bound; 
  m1.init <- Mlglu.mdd_dup m.init; 
  let new_reachset =  match m.reachset with
      Some b -> Some (Mlglu.mdd_dup b)
    | None -> None
  in m1.reachset <- new_reachset;
  m1.iinv <- Mlglu.mdd_dup m.iinv; 
  m1.oinv <- Mlglu.mdd_dup m.oinv; 
  (* dups the state sets *)
  let f (n: string) (mdd: Mlglu.mdd) : unit = 
     Hsetmap.add m1.ssets n (Mlglu.mdd_dup mdd) 
  in
  Hsetmap.iter f m.ssets; 
  (* dups the rules *)
  iter_irules m (add_rule m1); 
  iter_lrules m (add_rule m1); 
  iter_orules m (add_rule m1); 
  (* all done *)
  m1

