val composition :
  Symprog.t ->
  (Symprog.t -> Symmod.t -> Mlglu.mdd -> Mlglu.mdd) ->
  ?result_name:string -> Symmod.t -> Symmod.t -> Symmod.t

exception Modules_not_composable
exception Incompatible_Modules
