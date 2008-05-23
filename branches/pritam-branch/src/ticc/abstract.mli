
val compute_abstract_trans: Symmod.t ->bool -> Vset.VS.t -> Vset.VS.t -> Mlglu.mdd list * Mlglu.mdd list
val ctl_e_f : Symmod.t -> Mlglu.mdd -> Mlglu.mdd -> bool
val find_winning_states : Mlglu.mdd list -> Mlglu.mdd ->  Vset.VS.t -> Mlglu.mdd list -> Mlglu.mdd
val split : Mlglu.mdd -> Mlglu.mdd ->  Mlglu.mdd list ->  Vset.VS.t -> Vset.VS.t -> Symmod.t -> Mlglu.mdd list -> Vset.varid_t
val create_unchngd_list : Symprog.t -> Vset.VS.t list -> Vset.VS.t -> Mlglu.mdd list
val update_unchngd_list : Symprog.t -> Vset.VS.t list -> Vset.varid_t -> Mlglu.mdd list -> Mlglu.mdd list
