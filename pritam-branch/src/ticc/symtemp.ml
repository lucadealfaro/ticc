exception ReleasingUnallocatedVariable
exception TooManyBits

let max_nbits = 64

(** The pool of temporary variables, sorted by number of bits. 
  Each entry in the array is a list of variable ids. *)
(** TO DO: have one pool for each manager *)
let pool = Array.make (max_nbits +1) []

(** Returns a variable with 2^[nbits] possible values. *)
let allocate mgr nbits : int =
  if (nbits > max_nbits) then
    raise TooManyBits;
  match pool.(nbits) with
    [] ->
      (* computes 2^nbits *)
      let range = 1 lsl nbits in
      (* creates a new variable *)
      let new_id = Mlglu.mdd_create_variables mgr [range] [] [1] in
      new_id
  | h::t ->
      (* remove [h] from the list *)
      pool.(nbits) <- t;
      h

(** Puts a temporary variable back to the pool. *)
let release mgr id : unit =
  let nbits = Mlglu.mdd_get_var_bits mgr id in
  (** This is the only step that requires more than constant time *)
  if List.mem id pool.(nbits) then
    raise ReleasingUnallocatedVariable;
  pool.(nbits) <- id :: pool.(nbits)
