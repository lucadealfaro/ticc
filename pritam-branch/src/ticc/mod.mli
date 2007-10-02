type t

val create_empty : string -> t
val get_name   : t -> string
val get_hvars  : t -> (string, Var.t) Hsetmap.t
val get_init   : t -> Ast.t list
val get_ssets  : t -> (string, Ast.t) Hsetmap.t
val get_iinv   : t -> Ast.t list
val get_oinv   : t -> Ast.t list
val get_lrules : t -> (string, Rule.lrule_t) Hsetmap.t
val get_irules : t -> (string, Rule.irule_t) Hsetmap.t
val get_orules : t -> (string, Rule.orule_t) Hsetmap.t

val iter_lvars  : t -> (Var.t -> unit) -> unit
val iter_vars   : t -> (Var.t -> unit) -> unit
val iter_lrules : t -> (Rule.lrule_t -> unit) -> unit
val iter_irules : t -> (Rule.irule_t -> unit) -> unit
val iter_orules : t -> (Rule.orule_t -> unit) -> unit
val iter_ssets  : t -> (string -> Ast.t -> unit) -> unit

val is_lvar_name_def : t -> string -> bool
val is_fvar_name_def : t -> string -> bool
val is_var_name_def  : t -> string -> bool
val is_sset_name_def : t -> string -> bool

val add_somevar : (string, Var.t) Hsetmap.t -> Var.t -> unit
val add_lvar : t -> Var.t -> unit
val add_fvar : t -> Var.t -> unit
val add_hvar : t -> Var.t -> unit
val add_var  : t -> Var.t -> unit
val add_iinv : t -> Ast.t -> unit
val add_oinv : t -> Ast.t -> unit
val add_init : t -> Ast.t -> unit
val add_sset : t -> string -> Ast.t -> unit
val add_lrule : t -> Rule.lrule_t -> unit
val add_orule : t -> Rule.orule_t -> unit
val add_irule : t -> Rule.irule_t -> unit

val set_iinv : t -> Ast.t list -> unit
val set_oinv : t -> Ast.t list -> unit
val set_init : t -> Ast.t list -> unit
val set_ssets : t -> (string, Ast.t) Hsetmap.t -> unit

val lookup_lvar : t -> string -> Var.t
val lookup_fvar : t -> string -> Var.t
val lookup_var  : t -> string -> Var.t
val lookup_sset : t -> string -> Ast.t

val get_lvars : t -> (string, Var.t) Hsetmap.t
val get_fvars : t -> (string, Var.t) Hsetmap.t
val get_vars  : t -> (string, Var.t) Hsetmap.t

val is_irule_name_def : t -> string -> bool
val is_orule_name_def : t -> string -> bool
val is_lrule_name_def : t -> string -> bool

val set_hvars : t -> unit
val print       : t -> unit
val print_debug : t -> unit
