(** Implementation of Hset, or hash sets, an imperative
    data structure implemented on top of Hsetmaps. *)

(**  Revision [$Id: hset.mli,v 1.3 2005/06/26 06:49:02 luca Exp $] *)

type 'a t = ('a, unit) Hsetmap.t
(** Type of such a set *)
val mk : unit -> 'a t 
(** Makes an (empty!) set *)
val add : 'a t -> 'a -> unit
(** Adds one element to the set *)
val remove : 'a t -> 'a -> unit
(** Removes one element from the set *)
val mem : 'a t -> 'a -> bool
(** Checks whether an element is in the set *)
val isempty : 'a t -> bool
(** Is the set empty? *)
val size : 'a t -> int
(** How big is the set? *)
val copy : 'a t -> 'a t
(** Copies the hset *)
val to_list : 'a t -> 'a list
(** Converts to list *)

(** Iterators *)

val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'c -> 'c) -> 'a t -> 'c -> 'c
val map  : ('a -> 'b) -> 'a t -> 'b t 

(** Set operations *)

val union :  'a t -> 'a t -> 'a t 
(** Union *)
val diff :   'a t -> 'a t -> 'a t 
(** Set difference *)
val inters : 'a t -> 'a t -> 'a t 
(** Intersection *)
