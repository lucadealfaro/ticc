module VarSet :
  sig
    type elt = int
    type t = Vset.VS.t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
type varid_t = Vset.varid_t
exception NoTimedSupport
exception Internal_error
val have_same_signature : Symmod.t -> Symmod.t -> bool
val epsilon_closure :
  Symprog.t -> Symmod.t -> Ops.stateset_t -> Ops.stateset_t
val epsilon_closure_pred : Symprog.t -> Symmod.t -> Ops.stateset_t
val refinement : Symprog.t -> Symmod.t -> Symmod.t -> Ops.stateset_t
val refines : Symprog.t -> Symmod.t -> Symmod.t -> bool
