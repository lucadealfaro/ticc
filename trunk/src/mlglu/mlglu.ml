(*
 * $Id: mlglu.ml,v 1.33 2006/01/28 05:35:06 vishwa Exp $
 *
 *)

type mdd_manager
type mdd
type mvar 
(*= {
  mvar_id      : int;      (* mvar id *) 
  name         : string;   (* mvar name *) 
  values       : int;      (* number of values mvar can take *)
  encode_length: int;      (* number of binary variables needed to
			      encode mvar *)
  bvars        : int list; (* list of bvarids that encode the mvar *)
  encoding     : int       (* Glu internal use only *)
 }*) 

type bvar
  (*  = {
  node   : mdd;  
  mvar_id: int   
      } *)
 
type bdd_package_type = CMU | CAL | CUDD

type bdd_gen_status = BDD_EMPTY | BDD_NONEMPTY

type bdd_hook_type = BDD_PRE_GC_HOOK | BDD_POST_GC_HOOK 
		       | BDD_PRE_REORDERING_HOOK | BDD_POST_REORDERING_HOOK

type bdd_aprox_dir = BDD_OVER_APPROX | BDD_UNDER_APPROX

type bdd_partition_type = BDD_CONJUNCTS | BDD_DISJUNCTS

type bdd_reorder_verbosity = BDD_APPROX_HB | BDD_APPROX_SP | BDD_APPROX_COMP 
			     | BDD_APPROX_UA | BDD_APPROX_RUA | BDD_APPROX_BIASED_RUA

type bdd_reorder_type = BDD_REORDER_SIFT | BDD_REORDER_WINDOW | BDD_REORDER_SAME
			  | BDD_REORDER_RANDOM | BDD_REORDER_RANDOM_PIVOT | BDD_REORDER_SIFT_CONVERGE
			  | BDD_REORDER_SYMM_SIFT | BDD_REORDER_SYMM_SIFT_CONV | BDD_REORDER_LINEAR
			  | BDD_REORDER_LINEAR_CONVERGE | BDD_REORDER_EXACT | BDD_REORDER_WINDOW2
			  | BDD_REORDER_WINDOW3 | BDD_REORDER_WINDOW4 | BDD_REORDER_WINDOW2_CONV
			  | BDD_REORDER_WINDOW3_CONV | BDD_REORDER_WINDOW4_CONV | BDD_REORDER_GROUP_SIFT
			  | BDD_REORDER_GROUP_SIFT_CONV | BDD_REORDER_ANNEALING | BDD_REORDER_GENETIC
			  | BDD_REORDER_LAZY_SIFT | BDD_REORDER_NONE

exception InvalidMgr
exception DifferentMgrs
exception NullArray

let _ = Callback.register_exception "invalid mgr" (InvalidMgr)
let _ = Callback.register_exception "diff mgr" (DifferentMgrs)
let _ = Callback.register_exception "null array" (NullArray)

external mdd_and            : mdd -> mdd -> int -> int -> mdd = "mlglu_and"
external mdd_and_with_limit : mdd -> mdd -> int -> int -> int -> mdd = "mlglu_and_with_limit"
external mdd_and_array      : mdd -> mdd list -> int -> int -> mdd = "mlglu_and_array"
 
external mdd_dup                : mdd -> mdd = "mlglu_dup"
external mdd_equal              : mdd -> mdd -> bool = "mlglu_equal"
external mdd_equal_mod_care_set : mdd -> mdd -> mdd -> bool = "mlglu_equal_mod_care_set"
external mdd_closest_cube       : mdd -> mdd -> (mdd * int) = "mlglu_closest_cube"
external mdd_free               : mdd -> unit = "mlglu_free"
external mdd_get_manager        : mdd -> mdd_manager = "mlglu_get_manager"
external mdd_is_tautology       : mdd -> int -> bool = "mlglu_is_tautology"
external mdd_ite                : mdd -> mdd -> mdd -> int -> int -> int -> mdd = "mlglu_ite"
external mdd_lequal             : mdd -> mdd -> int -> int -> bool = "mlglu_lequal"
external mdd_lequal_mod_care_set: mdd -> mdd -> int -> int -> mdd -> bool = "mlglu_lequal_mod_care_set"
external mdd_lequal_array       : mdd -> mdd list -> int -> int -> bool = "mlglu_lequal_array"
external mdd_multiway_and       : mdd_manager -> mdd list -> mdd = "mlglu_multiway_and"
external mdd_multiway_or        : mdd_manager -> mdd list -> mdd = "mlglu_multiway_or"
external mdd_multiway_xor       : mdd_manager -> mdd list -> mdd = "mlglu_multiway_xor"
external mdd_not                : mdd -> mdd = "mlglu_not"
external mdd_or                 : mdd -> mdd -> int -> int -> mdd = "mlglu_or"
external mdd_size               : mdd -> int = "mlglu_size"
external mdd_size_multiple      : mdd list -> int = "mlglu_size_multiple"
external mdd_top_var_id         : mdd -> int = "mlglu_top_var_id"
external mdd_xor                : mdd -> mdd -> mdd = "mlglu_xor"
external mdd_xnor               : mdd -> mdd -> mdd = "mlglu_xnor"
external mdd_one                : mdd_manager -> mdd = "mlglu_one"
external mdd_zero               : mdd_manager -> mdd = "mlglu_zero"

external mdd_init       : int list -> string list -> int list -> mdd_manager = "mlglu_init"
external mdd_init_empty : unit -> mdd_manager = "mlglu_init_empty"

external mdd_create_variables : mdd_manager -> int list -> string list -> int list -> int = "mlglu_create_variables"
external mdd_create_variables_after : mdd_manager -> int -> int list -> string list -> int list -> int = "mlglu_create_variables_after"
external mdd_create_variables_interleaved : mdd_manager -> int -> int -> string list -> int = "mlglu_create_variables_interleaved"

external mdd_case       : mdd_manager -> int -> int list -> mdd = "mlglu_case"
external mdd_literal    : mdd_manager -> int -> int list -> mdd = "mlglu_literal"

external mdd_range_mdd_list  : mdd_manager -> int list -> mdd = "mlglu_range_mdd"
let mdd_range_mdd mgr vset = mdd_range_mdd_list mgr (Vset.to_list vset) 

external mdd_quit       : mdd_manager -> unit = "mlglu_quit"
external mdd_array_free : mdd list -> unit = "mlglu_array_free"
external mdd_array_duplicate : mdd list -> mdd list = "mlglu_array_duplicate"
external mdd_array_equal: mdd list -> mdd list -> bool = "mlglu_array_equal"
external mdd_func1c     :mdd_manager -> int -> int -> (int -> int -> bool) -> mdd = "mlglu_func1c" 
external mdd_func2      :mdd_manager -> int -> int -> (int -> int -> bool) -> mdd = "mlglu_func2"
external mdd_func2c     :mdd_manager -> int -> int -> int -> (int -> int -> int -> bool) -> mdd = "mlglu_func2c"
external mdd_func3      :mdd_manager -> int -> int -> int -> (int -> int -> int -> bool) -> mdd = "mlglu_func3"

external mdd_eq_s  : mdd_manager -> int -> int -> mdd = "mlglu_eq_s"
external mdd_neq_s : mdd_manager -> int -> int -> mdd = "mlglu_neq_s"
external mdd_leq_s : mdd_manager -> int -> int -> mdd = "mlglu_leq_s"
external mdd_lt_s  : mdd_manager -> int -> int -> mdd = "mlglu_lt_s"
external mdd_geq_s : mdd_manager -> int -> int -> mdd = "mlglu_geq_s"
external mdd_gt_s  : mdd_manager -> int -> int -> mdd = "mlglu_gt_s"
external mdd_eq_c  : mdd_manager -> int -> int -> mdd = "mlglu_eq_c"
external mdd_neq_c : mdd_manager -> int -> int -> mdd = "mlglu_neq_c"
external mdd_leq_c : mdd_manager -> int -> int -> mdd = "mlglu_leq_c"
external mdd_lt_c  : mdd_manager -> int -> int -> mdd = "mlglu_lt_c"
external mdd_geq_c : mdd_manager -> int -> int -> mdd = "mlglu_geq_c"
external mdd_gt_c  : mdd_manager -> int -> int -> mdd = "mlglu_gt_c"
external mdd_eq    : mdd_manager -> int -> int -> mdd = "mlglu_eq"
external mdd_neq   : mdd_manager -> int -> int -> mdd = "mlglu_neq"
external mdd_leq   : mdd_manager -> int -> int -> mdd = "mlglu_leq"
external mdd_lt    : mdd_manager -> int -> int -> mdd = "mlglu_lt"
external mdd_geq   : mdd_manager -> int -> int -> mdd = "mlglu_geq"
external mdd_gt    : mdd_manager -> int -> int -> mdd = "mlglu_gt"

external mdd_eq_plus_c  : mdd_manager -> int -> int -> int -> mdd = "mlglu_eq_plus_c"
external mdd_neq_plus_c : mdd_manager -> int -> int -> int -> mdd = "mlglu_neq_plus_c"
external mdd_leq_plus_c : mdd_manager -> int -> int -> int -> mdd = "mlglu_leq_plus_c"
external mdd_lt_plus_c  : mdd_manager -> int -> int -> int -> mdd = "mlglu_lt_plus_c"
external mdd_geq_plus_c : mdd_manager -> int -> int -> int -> mdd = "mlglu_geq_plus_c"
external mdd_gt_plus_c  : mdd_manager -> int -> int -> int -> mdd = "mlglu_gt_plus_c"
external mdd_eq_minus_c_mod  : mdd_manager -> int -> int -> int -> mdd = "mlglu_eq_minus_c_mod"
external mdd_neq_minus_c_mod : mdd_manager -> int -> int -> int -> mdd = "mlglu_neq_minus_c_mod"
external mdd_leq_minus_c_mod : mdd_manager -> int -> int -> int -> mdd = "mlglu_leq_minus_c_mod"
external mdd_lt_minus_c_mod  : mdd_manager -> int -> int -> int -> mdd = "mlglu_lt_minus_c_mod"
external mdd_geq_minus_c_mod : mdd_manager -> int -> int -> int -> mdd = "mlglu_geq_minus_c_mod"
external mdd_gt_minus_c_mod  : mdd_manager -> int -> int -> int -> mdd = "mlglu_gt_minus_c_mod"
external mdd_eq_plus   : mdd_manager -> int -> int -> int -> mdd = "mlglu_eq_plus"
external mdd_neq_plus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_neq_plus"
external mdd_leq_plus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_leq_plus"
external mdd_lt_plus   : mdd_manager -> int -> int -> int -> mdd = "mlglu_lt_plus"
external mdd_geq_plus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_geq_plus"
external mdd_gt_plus   : mdd_manager -> int -> int -> int -> mdd = "mlglu_gt_plus"
external mdd_eq_minus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_eq_minus"
external mdd_neq_minus : mdd_manager -> int -> int -> int -> mdd = "mlglu_neq_minus"
external mdd_leq_minus : mdd_manager -> int -> int -> int -> mdd = "mlglu_leq_minus"
external mdd_lt_minus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_lt_minus"
external mdd_geq_minus : mdd_manager -> int -> int -> int -> mdd = "mlglu_geq_minus"
external mdd_gt_minus  : mdd_manager -> int -> int -> int -> mdd = "mlglu_gt_minus"

external mdd_cofactor          : mdd_manager -> mdd -> mdd -> mdd = "mlglu_cofactor"
external mdd_cofactor_minterm  : mdd -> mdd -> mdd = "mlglu_cofactor_minterm"

external mdd_smooth_original   : mdd_manager -> mdd -> int list -> mdd
  = "mlglu_smooth"

let mdd_smooth_list mgr a vset =
  if List.length vset > 0 then
    mdd_smooth_original mgr a vset
  else 
    a

let mdd_smooth mgr a vset = mdd_smooth_list mgr a (Vset.to_list vset)

external mdd_and_smooth_original : mdd_manager -> mdd -> mdd -> int list -> mdd 
  = "mlglu_and_smooth"

let mdd_and_smooth_list mgr a b vset =
  if List.length vset > 0 then
    mdd_and_smooth_original mgr a b vset
  else 
    mdd_and a b 1 1
      
let mdd_and_smooth mgr a b vset = mdd_and_smooth_list mgr a b (Vset.to_list vset)

external mdd_consensus_list   : mdd_manager -> mdd -> int list -> mdd = "mlglu_consensus"
let mdd_consensus mgr a vset = mdd_consensus_list mgr a (Vset.to_list vset)

external mdd_substitute_two_lists : mdd_manager -> mdd -> int list -> int list -> mdd = "mlglu_substitute"
let mdd_substitute mgr a (old_new_list: (int * int) list) = 
  let (l1, l2) = List.split old_new_list in mdd_substitute_two_lists mgr a l1 l2 

external mdd_get_support_list : mdd_manager -> mdd -> int list = "mlglu_get_support"
let mdd_get_support mgr a = 
  let sup_list = mdd_get_support_list mgr a in 
  let f id_set id = Vset.VS.add id id_set in 
  List.fold_left f Vset.VS.empty sup_list 

external mdd_cproject_list    : mdd_manager -> mdd -> int list -> mdd = "mlglu_cproject"
let mdd_cproject mgr a vset = mdd_cproject_list mgr a (Vset.to_list vset)

external mdd_add_s            : mdd_manager -> int -> int -> int -> mdd = "mlglu_add_s"
external mdd_mod              : mdd_manager -> int -> int -> int -> mdd = "mlglu_mod"
external build_leq_c          : mdd_manager -> int -> int -> mdd = "mlglu_build_leq_c"
external build_lt_c           : mdd_manager -> int -> int -> mdd = "mlglu_build_lt_c"
external build_geq_c          : mdd_manager -> int -> int -> mdd = "mlglu_build_geq_c"
external build_gt_c           : mdd_manager -> int -> int -> mdd = "mlglu_build_gt_c"

external mdd_bundle_variables_list : mdd_manager -> int list -> string -> (int * int) = "mlglu_bundle_variables"
let mdd_bundle_variables mgr vset str = mdd_bundle_variables_list mgr (Vset.to_list vset) str

external mdd_ret_bvar         : mvar -> int -> bvar list -> bvar = "mlglu_ret_bvar"
external mdd_ret_bvar_id      : mvar -> int -> int = "mlglu_ret_bvar_id"
external mdd_ret_mvar_list    : mdd_manager -> mvar list = "mlglu_ret_mvar_list"
external mdd_ret_bvar_list    : mdd_manager -> bvar list = "mlglu_ret_bvar_list"

external mdd_get_var_bits     : mdd_manager -> int -> int = "mlglu_get_var_bits"
external mdd_get_var_range    : mdd_manager -> int -> int = "mlglu_get_var_range"
external mdd_get_var_name    : mdd_manager -> int -> string = "mlglu_get_var_name"

external mdd_dynamic_reordering: mdd_manager -> bdd_reorder_type -> bdd_reorder_verbosity -> unit = "mlglu_dynamic_reordering"

external mdd_print: mdd_manager -> mdd -> unit = "mlglu_print_mdd" 
external mdd_to_string: mdd_manager -> mdd -> string = "mlglu_mdd_to_string" 

(* This function has been imported from glu-chai. It is used to do random simulation *)
external mdd_pick_one_minterm_list   : mdd_manager -> mdd -> int list -> mdd = "mlglu_pick_one_minterm"
let mdd_pick_one_minterm mgr a vset = mdd_pick_one_minterm_list mgr a (Vset.to_list vset)

external mdd_pick_one_cube_list   : mdd_manager -> mdd -> int list -> mdd = "mlglu_pick_one_cube"
let mdd_pick_one_cube mgr a vset = mdd_pick_one_cube_list mgr a (Vset.to_list vset)
