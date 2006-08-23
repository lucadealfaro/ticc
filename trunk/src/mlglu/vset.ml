(** Here are the functions that implement the sets of variables *) 

module type OInt = 
  sig
    type t = int 
    val compare: t -> t -> int 
  end    

module OrderedInt: OInt = 
  struct
    type t = int 
      (** The type of the set elements. *)
    let compare (n: int) (m: int) : int = n - m 
      (** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
  end;;


(** This is tye type of a set of variables. 
    In other modules, all you need to do is: 
    module VarSet = Vset.IntegerSet *) 
module VS = Set.Make (OrderedInt);; 

(** Type of a variable id *) 
type varid_t = int;; 

(** This converts a set of variables to a list, and is used especially
    in mlglu.ml *) 
let to_list (var_set: VS.t) : varid_t list = 
  let f id l = id :: l in 
  VS.fold f var_set [] 


