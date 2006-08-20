(*
 * House example, version 2, partial composition. 
 *)

open Ticc;;

parse "house3.si";;

let sp = Symprog.toplevel;;
let mgr = Symprog.get_mgr sp;;
let one = Mlglu.mdd_one mgr;;
print_string "Ora provo\n";;
flush stdout;;
let sup1 = Mlglu.mdd_get_support mgr one;;

