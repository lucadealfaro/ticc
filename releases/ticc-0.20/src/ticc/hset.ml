(** $Id: hset.ml,v 1.4 2005/06/26 06:49:19 luca Exp $ *)

(** Implementation of Hset, or hash sets, an imperative
    data structure implemented on top of Hsetmaps. *)

type 'a t = ('a, unit) Hsetmap.t ;;

(** Methods *)

let mk       = Hsetmap.mk ;; 
let add s el = Hsetmap.add s el () ;;
let remove   = Hsetmap.remove ;; 
let mem      = Hsetmap.mem ;; 
let isempty  = Hsetmap.isempty ;; 
let size     = Hsetmap.size ;; 
let copy     = Hsetmap.copy ;;

(** Iterators *) 

let iter f s = 
  let g key el = f key 
  in Hsetmap.iter g s ;; 

let fold f e v = 
  let g key el u = f key u
  in Hsetmap.fold g e v ;; 

let map f s = 
  let s' = mk () in 
  let g el = add s' (f el) in 
  iter g s; 
  s' 

(** Conversion *)
let to_list s =
  let add_one el l = el::l in
  fold add_one s []

(** Set methods *)

let union     = Hsetmap.unsafe_union        ;; 
let diff      = Hsetmap.unsafe_difference   ;; 
let inters    = Hsetmap.unsafe_intersection ;;

(* let equal_via_biject = Hsetmap.equal_via_biject ;; *)
