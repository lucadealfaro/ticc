(** This module contains the algorithms which deal with
  non-zenoness. *)


(** [i_live sm] returns the set of I-live states of module [sm].
  If the optional labelled argument [verbose] is set to true,
  the function provides a more detailed progress indicator. *)
val i_live : Symprog.t -> ?verbose:bool -> Symmod.t -> Mlglu.mdd

(** [o_live sm] returns the set of O-live states of module [sm].
  If the optional labelled argument [verbose] is set to true,
  the function provides a more detailed progress indicator. *)
val o_live : Symprog.t -> ?verbose:bool -> Symmod.t -> Mlglu.mdd

(** [i_live_alt sm] returns the set of I-live states of module [sm],
  using an alternative algorithm (Jurdzinski's progress measure
  algorithm). *)
val i_live_alt : Symprog.t -> Symmod.t -> Mlglu.mdd

(** Winning set of the composition game.
  This function is for Ticc internal use.

  Let good be the set of locally compatible states.
  The composition game has goal:

     (Always good) and (time divergence or blame Output)

  We first solve the safety goal (Always good).
  Then, we solve the liveness goal while forcing Input to always
  stay in the safety winning set.
*)
val win_composition :
  Symprog.t -> Symmod.t -> Symmod.stateset_t -> Symmod.stateset_t
