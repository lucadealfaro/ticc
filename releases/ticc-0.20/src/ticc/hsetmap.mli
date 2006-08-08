(** $Id: hsetmap.mli,v 1.7 2005/10/28 19:30:00 luca Exp $ *)

(** A Hsetmap is a hashtable with set methods defined for it. 
   Like a hashtable, given a key it can retrieve the data.
   Like a set, it has union, set difference, and intersection defined.
   This version is imperative, i.e., it has in-place modification.
   Also, since I am at it, I add the size of the set.  It's silly to
   have to traverse the set to know it! 
   This module uses a generic hashtable implementation, so if you are 
   very concerned about hashing efficiency, you may be better off 
   doing it from scratch. *)

(** Type of a Hsetmap from 'a to 'b *)
type ('a, 'b) t

(** creates a new mapping *)
val mk : unit -> ('a, 'b) t

(** empties it in a single blow *) 
val erase: ('a, 'b) t -> unit 

(** Adds an element to the mapping, and complains if it's there *)
val add_new : ('a, 'b) t -> 'a -> 'b -> unit
(** Adds an element to the mapping, unless it's already there *)
val add : ('a, 'b) t -> 'a -> 'b -> unit

(** Adds an element to the mapping.
    If the key is already there, it modifies the data. *)
val modify : ('a, 'b) t -> 'a -> 'b -> unit

(** Adds an element to the mapping. 
    If the element is already there, it combines it with the existing
    element. *)
val add_combine : ('a, 'b) t -> ('b -> 'b -> 'b) -> 'a -> 'b -> unit

(** Removes an element from the mapping given the key *)
val remove : ('a, 'b) t -> 'a -> unit

(** Checks if a key is in the domain of a mapping *)
val mem : ('a, 'b) t -> 'a -> bool

(** Finds the data associated with a given key.
    Can raise 'Not_found'.  *)
val find : ('a, 'b) t -> 'a -> 'b

(** Is the mapping empty? *)
val isempty : ('a, 'b) t -> bool

(** size of the mapping *) 
val size : ('a, 'b) t -> int

(** makes a copy of a mapping *) 
val copy : ('a, 'b) t -> ('a, 'b) t

(** Iterators *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** This is a special iterator that iterates only on the 
    "body" (not the key). *)
val iter_body : ('b -> unit) -> ('a, 'b) t -> unit 

(** Fold *)
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** Folds over the body only *)
val fold_body: ('b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

(** Returns a list containing all the values in the set. *)
val body_to_list : ('a, 'b) t -> 'b list

(** Set methods, implemented on top of the above methods. 
    Combinator functions are used to take care of the case when 
    the data associated with the key does not match.
    Unsafe methods do without this; be careful using them!! *) 

(** Unsafe methods *)

val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

val unsafe_union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val unsafe_difference : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val unsafe_intersection : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(** Safe methods *)

(** [union h1 h2 f] returns a mapping that represents the union of
   [h1] and [h2].  If a key [k] is present in both [h1] and [h2], 
   mapped to [d1] and [d2], then in the resulting mapping [k] is 
   mapped to [f d1 d2]. *)
val union : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> 'b) -> ('a, 'b) t

(** [difference m1 m2 f] returns a mapping that represents the set 
    difference of [m1] and [m2].
    If [k] is in [m1] but not in [m2], [k] is in the result. 
    If [k] is in both [m1] and [m2], and the data associated to [k] in  
    [m1] and [m2] matches, then [k] is not in the final result.
    If [k] is in both [m1] and [m2], and the data associated to [k] in  
    [m1] and [m2] does not match, then [f] is used.
    [f] returns a type ['a option], and the outcome is inserted 
    only if it returns something. *) 
val difference :
  ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> 'b option) -> ('a, 'b) t

(** Computes the intersection of two mappings. 
   In [intersection m1 m2 f], the function [f] 
   is used to combine elements that appear both in m1 and in m2. *)
val intersection : ('a, 'b) t -> ('a, 'c) t -> ('b -> 'c -> 'd) -> ('a, 'd) t

(** [add_to_first m1 m2] adds all elements of m2 whose key is not in m1 to m1 *)
val add_to_first : ('a, 'b) t -> ('a, 'b) t -> unit
