(** Module Biject, implementing bijections. *)

type ('a, 'b) t = {
    first:  ('a, 'b) Hsetmap.t; 
    second: ('b, 'a) Hsetmap.t
  } ;; 

let mk () = {
  first  = Hsetmap.mk (); 
  second = Hsetmap.mk ()
};; 

(** [add m el1 el2] adds [(el1, el2)] to mapping [m],
    provided that el1 is not in the domain of [m]
    and el3 is not in its range. *)
let add m el1 el2 = 
  begin
    Hsetmap.add m.first  el1 el2 ; 
    Hsetmap.add m.second el2 el1
  end ;; 

let map_first  m el  = Hsetmap.find m.first  el 

let map_second m el  = Hsetmap.find m.second el  

let mem_first m el   = Hsetmap.mem  m.first  el

let mem_second m el  = Hsetmap.mem  m.second el

let get_map     m = m.first ;;
let get_inverse m = m.second ;;

let size m = Hsetmap.size m.first 

let isempty m = Hsetmap.isempty m.first 

let copy m = {
  first  = Hsetmap.copy m.first; 
  second = Hsetmap.copy m.second
};; 
   


let unsafe_union m1 m2 =
  let new_first  = Hsetmap.unsafe_union m1.first m2.first in
  let new_second = Hsetmap.unsafe_union m1.second m2.second in
  {first = new_first; second = new_second}


let remove_first m el1 = 
  let el2 = (map_first m el1) in 
  begin
    Hsetmap.remove m.first  el1; 
    Hsetmap.remove m.second el2;
  end ;; 

let remove_second m el2 = 
  let el1 = (map_second m el2) in 
  begin
    Hsetmap.remove m.first  el1; 
    Hsetmap.remove m.second el2
  end ;; 

(** Iterators: Work on first -> second mappings. *)

let iter_first f m = Hsetmap.iter f m.first

let fold_first f m b = Hsetmap.fold f m.first b 
  
(** Iterators: Work on second -> first mappings. *)

let iter_second f m = Hsetmap.iter f m.second

let fold_second f m b = Hsetmap.fold f m.second b 
 
 
(* [compose_inv bij1 and bij2] composes the bijections [bij1] and
[bij2]^-1,
i.e. it maps x to z iff bij1(x) = y and bij2(y) = z for some y. 
let compose_inv bij1 bij2 =
  let new_map = mk ()
  in
    let add_new_pair el1 el2 = add new_map el1 (map_second bij2 el2)
    in
    iter_first add_new_pair bij1;
    new_map
;;
*)
 
(** Consistency check *)

exception Broken_bijection;; 

let check_bijection m = 
  if Hsetmap.size m.first = Hsetmap.size m.second then 
    let f el1 el2 =
      let el1' = map_second m el2 in 
      if el1' != el1 then raise Broken_bijection
    in
    Hsetmap.iter f m.first 
  else
    raise Broken_bijection;; 

(** Tostring method *) 
let tostr ta tb m = 
  let f x y s = "(" ^ (ta x) ^ "," ^ (tb y) ^ ")\n " ^ s 
  in fold_first f m "";;
