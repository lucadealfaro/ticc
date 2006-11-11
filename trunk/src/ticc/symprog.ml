(** symprog.ml *)

(** This declares the symbolic top level, and contains functions to
    add variables, etc. *)

module VarSet = Vset.VS

type varid_t = Vset.varid_t 

(** This is the type of a symbolic toplevel *)
type t = {
  name  : string;

  (** MDD manager *)
  mgr: Mlglu.mdd_manager; 

  (** blame variables *)
  bli: varid_t;
  blo: varid_t;

  (** These are GLOBAL mappings, also for variables local to 
      the modules. *)
  (** The following is the best way to do the mappings. 
      Insipired by Freddy's work in Mocha. *)
  (** Maps a variable to a pair consisting of the unprimed 
      and primed MDD ids *)
  var_to_ids: (Var.t, (varid_t * varid_t)) Hsetmap.t; 
  (** Maps an MDD id to a pair, consisting of a variable, and 
      of a flag telling us whether the variable is primed. *)
  id_to_var_p: (varid_t, (Var.t * bool)) Hsetmap.t; 
  (** Bijection between unprimed and primed variable ids *)
  id_to_pid: (varid_t, varid_t) Biject.t; 
  (** Bijection between unprimed and temporary (also called "extra") variable ids.
      Temporary variables are used by refinement. *)
  id_to_tid: (varid_t, varid_t) Biject.t;
}

exception ID_not_unprimed
exception ID_not_primed
exception Var_not_defined
exception ID_not_defined

(** This is the name of an environment action *)
let env_act = "_env_action"

(** Creates an empty symbolic toplevel *)
let mk (n: string) : t = 
  let mgr = Mlglu.mdd_init [2; 2] ["bli"; "blo"] [1; 1] in
  let bli = 0 in
  let blo = 1 in
  {
    name = n; 

    mgr = mgr;

    bli = 0;
    blo = 1;

    var_to_ids  = Hsetmap.mk ();
    id_to_var_p = Hsetmap.mk ();
    id_to_pid   = Biject.mk (); 
    id_to_tid   = Biject.mk (); 
  }

(** This is the symbolic toplevel we use by default *)
let toplevel = mk "Toplevel"

(** Gets mdd manager *)
let get_mgr (s: t) : Mlglu.mdd_manager = s.mgr

(** ********** Variables ********************************** *)

(** Gets the ID for a (variable, primed)
    s: symbolic program 
    v: variable 
    p: flag indicating whether primed *) 
let get_id_of_var (s: t) v p = 
  if Hsetmap.mem s.var_to_ids v 
  then begin
    let (id, pid) = Hsetmap.find s.var_to_ids v in 
    if p then pid else id
  end else begin
    Printf.printf "\nVariable %s not defined\n" (Var.get_name v);
    flush stdout; 
    raise Var_not_defined
  end

(** Gets the set of (unprimed) IDs for a set of variables. *)
let get_id_of_varset (s: t) (vset: (string, Var.t) Hsetmap.t) : VarSet.t =
  let f n v id_set =
    let vid = get_id_of_var s v false in
    VarSet.add vid id_set 
  in
  Hsetmap.fold f vset VarSet.empty 

(** Checks if an ID is primed *)
let is_id_primed s (id: varid_t)  = 
  let (v, p) = Hsetmap.find s.id_to_var_p id in p

(** Primes an ID *)
let prime_id s (id: varid_t) : varid_t = 
  if Biject.mem_first s.id_to_pid id 
  then Biject.map_first s.id_to_pid id 
  else raise ID_not_unprimed

(** Unprimes an ID *)
let unprime_id s (id: varid_t) : varid_t = 
  if Biject.mem_second s.id_to_pid id 
  then Biject.map_second s.id_to_pid id 
  else raise ID_not_primed

(** Primes a set of vars
    Ok, this is trivial to write, but since we will use it 
    so often... *) 
let prime_vars s (vs: VarSet.t) : VarSet.t = 
  let addone id = VarSet.add (prime_id s id) in 
  VarSet.fold addone vs VarSet.empty 

(** Unprimes a set of variables *) 
let unprime_vars s (vs: VarSet.t) : VarSet.t = 
  let addone id = VarSet.add (unprime_id s id) in 
  VarSet.fold addone vs VarSet.empty 

(** Gets the (variable, primed) for an ID *)
let get_var_p s (id: varid_t) : (Var.t * bool) = 
  if Hsetmap.mem s.id_to_var_p id 
  then Hsetmap.find s.id_to_var_p id 
  else raise ID_not_defined

(** Checks whether a variable is present *)
let is_var_def s v = Hsetmap.mem s.var_to_ids v 

(** ********** Support for parity games *********************** *)
let get_bli s = s.bli
let get_blo s = s.blo


(** Adds interleaved variables to the manager (does nothing for 
    variables that are already defined in the manager).
    The binary encodings of the variables in a single call to
    add_var_list are interleaved, so one call should be done for each
    group of related variables. 
    As a heuristics, this function should be called as follows: 
    1) A single call, passing the list of all the clocks; 
    2) A single call, for any variable groups that are related; 
    3) One call per unrelated variable.
    Note that this creates mdd variables both for the primed, 
    and for the unprimed, variables. 
 *)
let add_var_list s (vl: Var.t list) : unit =
    (* Filters out the variables that are already present *)
  let nvl = List.filter (function v -> not (is_var_def s v)) vl in 

  (* apparently mdd_create_variables goes in segmentation fault if
     provided with an empty list (not anymore) *)
  if (List.length nvl) > 0 then begin
    (* The binary encodings of the variables are interleaved *)
    (* we have 2 * because we insert also primed variables *) 
    let stride = 2 * List.length nvl in 
    let build_lists (name_l, nvals_l, strides_l) v = 
      ((Var.get_name v) :: ((Var.get_name v) ^ "'") :: name_l, 
      (Var.nvals v) :: (Var.nvals v) :: nvals_l, 
      stride :: stride :: strides_l) in 
    (* We reverse the nvl so that variables are actually added in the
       order we specify *)
    let (name_l, nval_l, strd_l) = List.fold_left build_lists ([], [], []) (List.rev nvl) in
    let first_id = Mlglu.mdd_create_variables s.mgr nval_l name_l strd_l in 
    (* Insert the variables and IDs in the various tables *)
    (* This function registers a variable and its primed version, with
       an id and an id + 1 *)
    let insert_var id v = 
      Hsetmap.add s.var_to_ids v (id, id + 1); 
      Hsetmap.add s.id_to_var_p id (v, false); 
      Hsetmap.add s.id_to_var_p (id + 1) (v, true); 
      Biject.add  s.id_to_pid id (id + 1) in 
    (* This function registers a list of variables, given its first id. *)
    let rec insert_var_list id = function 
	[] -> ()
      | v :: vl' ->
	  insert_var id v; 
	  insert_var_list (id + 2) vl'
    in 
    insert_var_list first_id nvl 
  end
;;


(** [get_extra_vars s vlist] returns a set of extra variables that
    correspond to the ones of [vlist].
    This function has to work with lists of variables, 
    rather than sets of variables, because correspondence
    between input variables and output variables must be kept.

    Such extra variables are created on demand and then "cached"
    for later use.
    By the way, there seems to be no way to remove variables
    from a manager in Glu.

    Currently, such variables are used by refinement. *)
let get_extra_vars (s: t) (vlist: varid_t list) : varid_t list =
  let add_one_var (id: int) cur_list : varid_t list =
    let tid = try 
      (* check if an extra variable already exists *)
      Biject.map_first s.id_to_tid id
    with Not_found ->
      let (var, _) = get_var_p s id in
      let new_name = [(Var.get_name var) ^ "''"] in
      let new_vals = [Var.nvals var] in
      let new_stride = [1] in
      let new_id = Mlglu.mdd_create_variables s.mgr 
	new_vals new_name new_stride in
      Biject.add s.id_to_tid id new_id;
      new_id
    in
    tid :: cur_list;
  in
  List.fold_right add_one_var vlist []
;;


(** [get_jurdzinski_var sp sm] returns the extra variable needed
    to run Jurdzinski's algorithm for parity games.

    (TO DO) Such extra variable is created on demand and then "cached"
    for later use. *)
let get_jurdzinski_var (sp: t) (sm: Symmod.t) =
  let nvals = ref 1 in
  let do_one_var var_id =
    let (var, _) = get_var_p sp var_id in
    nvals := !nvals * (Var.nvals var)
  in
  let vars = Symmod.get_vars sm in
  let cvars = Symmod.get_cvars sm in
  (* we do not need to consider states with different values of clocks
     as they are not reachable without passing 
     through states of color 0 *)
  let not_cvars = VarSet.diff vars cvars in
  VarSet.iter do_one_var not_cvars;
  (* add one, to represent "infinity" *)
  nvals := !nvals + 1;

(* we add two such variables;
   one is needed to increment the other by 1 *)
  let new_name = ["measure_" ^ (Symmod.get_name sm); 
  "measure2_" ^ (Symmod.get_name sm) ] in
  let new_vals = [!nvals; !nvals] in
  let new_stride = [1; 1] in
  let new_id = Mlglu.mdd_create_variables sp.mgr 
    new_vals new_name new_stride in
  (* Biject.add s.module_to_supervar sm new_id; *)
  (new_id, new_id +1)

(** Print functions *) 

(** print_iinv [sp] [sm] 
    Prints the input invariant of a module [sm], in the symbolic top
    level [sm]. **)
let print_iinv sp sm : unit = 
  let mgr = get_mgr sp in
  Mlglu.mdd_print mgr (Symmod.get_iinv sm)

(** print_oinv [sp] [sm] 
    Prints the input invariant of a module [sm], in the symbolic top
    level [sm]. **)
let print_oinv sp sm : unit = 
  let mgr = get_mgr sp in
  Mlglu.mdd_print mgr (Symmod.get_oinv sm)

(** [print_all_vars sp]
  Prints all the variables in the program [sp].
  Useful for debugging. **)
let print_all_vars sp : unit =
  Printf.printf "%d : %s\n" 0 (Mlglu.mdd_get_var_name sp.mgr 0);
  Printf.printf "%d : %s\n" 1 (Mlglu.mdd_get_var_name sp.mgr 1);
  let print_one (id: int) (var, p) : unit =
    Printf.printf "%d : %s\n" id (Var.get_name var);
  in
  Hsetmap.iter print_one sp.id_to_var_p
