(* $Id: biject.mli,v 1.1 2005/04/27 23:26:05 luca Exp $ *)

(** Module Biject, implementing bijections. *)

type ('a, 'b) t
val mk            :  unit -> ('a, 'b) t
val add           : ('a, 'b) t -> 'a -> 'b -> unit

(** Can raise Not_found *)
val map_first     : ('a, 'b) t -> 'a -> 'b
(** Can raise Not_found *)
val map_second    : ('a, 'b) t -> 'b -> 'a

val mem_first     : ('a, 'b) t -> 'a -> bool
val mem_second    : ('a, 'b) t -> 'b -> bool
val size          : ('a, 'b) t -> int
val get_map       : ('a, 'b) t -> ('a, 'b) Hsetmap.t
val get_inverse   : ('a, 'b) t -> ('b, 'a) Hsetmap.t
val isempty       : ('a, 'b) t -> bool
val copy          : ('a, 'b) t -> ('a, 'b) t
(*val compose_inv : ('a, 'b) t -> ('a, 'b) t -> ('a, 'a) t*)
val remove_first  : ('a, 'b) t -> 'a -> unit
val remove_second : ('a, 'b) t -> 'b -> unit

val unsafe_union  : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val iter_first    : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold_first    : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val iter_second   : ('a -> 'b -> unit) -> ('b, 'a) t -> unit
val fold_second   : ('a -> 'b -> 'c -> 'c) -> ('b, 'a) t -> 'c -> 'c

exception Broken_bijection
val check_bijection : ('a, 'b) t -> unit
val tostr         : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
