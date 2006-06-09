(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Modified and extended by: Luca de Alfaro, 2005. 
   For the modifications, Copyright 2005, Luca de Alfaro. *)

(* Hash tables *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data }

let length h = h.size

let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons(k, d, rest) ->
      compare k key = 0 || mem_in_bucket rest in
  mem_in_bucket h.data.((hash key) mod (Array.length h.data))

(* Notice that, differently from the ordinary Hashtbl.add, 
   this add will add only the first copy of the key to the set. 
   If you want that the last copy be kept, use modify instead. *) 

let add h key info =
  if not (mem h key) then begin
    let i = (hash key) mod (Array.length h.data) in
    let bucket = Cons(key, info, h.data.(i)) in
    h.data.(i) <- bucket;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h
  end

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if compare k key = 0
        then begin h.size <- pred h.size; next end
        else Cons(k, i, remove_bucket next) in
  let i = (hash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key = function
    Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if compare key k = 0 then d else find_rec key rest

let find h key =
  match h.data.((hash key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if compare key k1 = 0 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if compare key k2 = 0 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if compare key k3 = 0 then d3 else find_rec key rest3

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if compare k key = 0
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.((hash key) mod (Array.length h.data))

let replace h key info =
  let rec replace_bucket = function
      Empty ->
        raise Not_found
    | Cons(k, i, next) ->
        if compare k key = 0
        then Cons(k, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = (hash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(* ================================================================ *)
(*                                                                  *)
(* Functions added by Luca de Alfaro                                *) 
(*                                                                  *)
(* ================================================================ *)

(** Initial size is 10 *)
let mk () = 
  { size = 0; data = Array.make 10 Empty }

(** If it's already there, add_new raises exception Invalid_argument. *)
let add_new m key el = 
  if mem m key
  then raise (Invalid_argument "key already bound in mapping") 
  else add m key el

(** If it's not there, inserts it, otherwise, replaces it *)
let modify m key el = 
  if mem m key
  then remove m key;
  add m key el

let isempty m = (m.size = 0)  

(** This is a special iterator that iterates only on the 
    "body" (not the key). *)
let iter_body f m = 
  let g k b = f b in 
  iter g m  

(** This is a special fold that folds only over the body 
    (not the key) *)
let fold_body f m = 
    let g k b = f b in 
    fold g m 

(** Returns a list containing all the values in the set. *)
let body_to_list m =
    let add_one_val _ v tmp = v::tmp in
    fold add_one_val m []

(** Set methods, implemented on top of the above methods. 
    Combinator functions are used to take care of the case when 
    the data associated with the key does not match.
    Unsafe methods do without this; be careful using them!! *) 

(** [compose m1 m2] composes two Hsetmaps *)
let compose m1 m2 =
  let new_map = mk () in
  let add_new_pair key1 el1 = add new_map key1 (find m2 el1)
  in
  iter add_new_pair m1;
  new_map

let size m = m.size

let erase m = 
  m.size <- 0; 
  m.data <- Array.make 10 Empty

(** Unsafe methods *)

let unsafe_union m1 m2 = 
  (* copy the largest, iterate on the smallest *)
  let (n1, n2) = if m1.size >= m2.size then (m1, m2) else (m2, m1) in 
  let n = copy n1 in 
  let insert key el = if not (mem n1 key) then add n key el in 
  begin 
    iter insert n2 ;
    n 
  end  

let unsafe_difference (m1 : ('a, 'b) t) (m2 : ('a, 'b) t) = 
  if m1.size <= m2.size then 
    (* We are removing _many_ elements from a small set. *)
    (* We iterate on the small set. *)
    begin
      let n = mk () in 
      let subtract key el = if not (mem m2 key) then add n key el in 
      begin
	iter subtract m1 ;
	n
      end
    end
  else 
    (* We are removing elements from a _large_ set. *)
    (* Make a copy, then iterate on the small set and remove them. *)
    begin
      let n = copy m1 in 
      let subtract key el = 
	try 
	  remove n key
	with Not_found -> ()
      in
      begin
	iter subtract m2 ;
	n
      end
    end 

let unsafe_intersection (m1 : ('a, 'b) t) (m2 : ('a, 'b) t) = 
  (* iterate on the smallest *)
  let (n1, n2) = if m1.size <= m2.size then (m1, m2) else (m2, m1) in 
  let n = mk () in 
  let intersect key el = if mem n2 key then add n key el in 
  begin 
    iter intersect n1 ;
    n
  end 

(** Safe methods *)

(** The function comb is used to combine data whose key matches. *) 
let union m1 m2 comb = 
  (* copy the largest, iterate on the smallest *)
  if m1.size >= m2.size then
    let n = copy m1 in 
    let insert key el2 = 
      try 
	let el1 = find m1 key in 
	begin
	  remove n key ; 
	  add n key (comb el1 el2) 
	end
      with Not_found -> 
	add n key el2 
    in 
    begin 
      iter insert m2 ;
      n 
    end
  else
    let n = copy m2 in 
    let insert key el1 = 
      try 
	let el2 = find m2 key in 
	begin
	  remove n key ; 
	  add n key (comb el1 el2) 
	end
      with Not_found -> 
	add n key el1 
    in 
    begin 
      iter insert m1 ;
      n 
    end 

(** If k is in m1 but not in m2, k is in the result. 
    If k is in both m1 and m2, and the data associated to k in  
    m1 and m2 matches, then k is not in the final result.
    If k is in both m1 and m2, and the data associated to k in  
    m1 and m2 does not match, then comb is used.
    comb must return a type 'a option, and the outcome is inserted 
    only if it returns something. *) 
let difference m1 m2 comb = 
  if m1.size <= m2.size then 
    (* We are removing _many_ elements from a small set. *)
    (* We iterate on the small set. *)
    begin
      let n = mk () in 
      let add_if_not_in_m2 key el1 = 
	try 
	  let el2 = find m2 key in 
	  if el1 != el2 then 
	      (* If they match, do nothing. Otherwise, use comb. *)
	    match (comb el1 el2) with 
	      None -> () 
	    | Some (el) -> add n key el 
	with Not_found -> 
	  add n key el1
      in 
      begin
	iter add_if_not_in_m2 m1 ;
	n
      end
    end
  else 
    (* We are removing elements from a _large_ set. *)
    (* Make a copy, then iterate on the small set and remove them. *)
    let n = copy m1 in 
    begin
      let subtract key el2 = 
	try 
	  let el1 = find m1 key in 
	  if el1 = el2 
	      (* If the key maps to the same elements, remove it. *)
	  then remove n key 
	      (* If the key maps to different elements, combine them,*)
	      (* and add to n the result (if any). *)
	  else 
	    match (comb el1 el2) with 
	      None -> remove n key
	    | Some (el) -> 
		begin
		  remove n key ; 
		  add    n key el
		end
	with Not_found -> ()
      in
      begin
	iter subtract m2 ;
	n
      end
    end 
 
(** The function comb is used to combine elements that appear both
    in m1 and in m2. *)
let intersection m1 m2 comb = 
  let n = mk () in 
  (* iterate on the smallest *)
  if m1.size <= m2.size
  then
    let intersect key el1 = 
      try 
	let el2 = find m2 key in 
	add n key (comb el1 el2) 
      with Not_found -> ()
    in 
    begin 
      iter intersect m1 ;
      n
    end
  else
    let intersect key el2 = 
      try 
	let el1 = find m1 key in 
	add n key (comb el1 el2) 
      with Not_found -> ()
    in 
    begin 
      iter intersect m2 ;
      n
    end 


(** [add_to_first m1 m2] adds all elements of m2 whose key is not in m1 to m1 *)
let add_to_first m1 m2 = 
  let f k v = 
    if not (mem m1 k) then add m1 k v
  in iter f m2 

