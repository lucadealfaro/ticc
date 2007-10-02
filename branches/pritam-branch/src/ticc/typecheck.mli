(* $Id: typecheck.mli,v 1.4 2005/05/01 07:55:46 luca Exp $ *)

exception TypeError

(** Type-checks an expression, updating also the clock bounds, and
    checking that the top level type is boolean. *)
val type_check_bool: Ast.t -> unit 

(** Type-checks a module declaration *)
val check_module : Mod.t -> unit

(** Optimizes the operations in a module, to speed up the construction
    of the MDDs, and to minimize the number of negative intermediate
    resuts. *)
val optimize_module : Mod.t -> unit 
