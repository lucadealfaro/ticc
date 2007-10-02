(** Provides temporary MDD variables to {!Symbuild}. *)

exception ReleasingUnallocatedVariable
exception TooManyBits

(** [allocate manager n] returns the id of a temporary
  variable belonging to [manager] and having range
  [0..(2^n -1)].
  Raises TooManyBits if [n] is larger than Symtemp.max_nbits. *)
val allocate : Mlglu.mdd_manager -> int -> int

(** [release manager id] releases the temporary variable 
  whose id is [id].
  The variable id should have been obtained from [allocate]. *)
val release  : Mlglu.mdd_manager -> int -> unit
