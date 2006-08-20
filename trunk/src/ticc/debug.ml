(*
 * House example, version 2, partial composition. 
 *)

open Ticc;;

parse "house3.si";;

let plumber     = mk_sym "Plumber";;
let electrician = mk_sym "Electrician";;
(*
let rudelectr   = mk_sym "RudeElectrician";;
let floors      = mk_sym "Floors";;
let walls       = mk_sym "Walls";;
let clean       = mk_sym "Clean";;
let breaks      = mk_sym "Breaks";;
let calls	= mk_sym "Calls";;
*)

let c_pr  = compose plumber electrician;;
(*
let d_pr  = compose plumber rudelectr;;
*)

(* For curiosity *)
(* print_symmod plumber;; *)
(* print_symmod_rules plumber "_env_action";; *)

(* Trouble here is the starting point of the transition that will lead
   to problems.  This stuff is here only for diagnostic purposes. *)
(*
let g                  = get_stateset rudelectr "RudeElectrician.guard";;
let plumber_in_kitchen = get_stateset plumber   "Plumber.plumber_in_kitchen";;
let trouble = set_and g plumber_in_kitchen;;

let reach_pr  = get_reachable d_pr;;
print_string "\nReachable trouble in plumber || rude_electrician: ";;
print_bool (set_is_nonempty (set_and trouble reach_pr);;
*)

(* r_c_pr is the restriction of call_electr in plumber || electrician *)
let r_c_pr = get_input_restriction c_pr "call_electr";;
print_string "\nRestriction of call_electr in  plumber || electrician :\n";;
print_bool (set_is_nonempty r_c_pr);;

print_restr_paths c_pr "call_electr" 1;;

(* r_d_pr is the restriction of call_electr in plumber || rudelectr *)
(*
let r_d_pr = get_input_restriction d_pr "call_electr";;
print_string "\nRestriction of call_electr in  plumber || rude_electrician :\n";;
print_bool (set_is_nonempty r_d_pr);;
*)





