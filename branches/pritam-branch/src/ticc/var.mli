type type_t
type t 

val clock_type : type_t
val bool_type : type_t
val range_type : int -> int -> type_t

val assert_int : t -> unit
val assert_bool : t -> unit

val mk : string -> type_t -> string option -> t
val mk_same_type : t -> string -> string option -> t

val get_name : t -> string
val get_bound : t -> int
val set_bound : t -> int -> unit
val get_mod_name : t -> string option
val is_global : t -> bool
val is_clock : t -> bool
val is_bool : t -> bool
val is_range : t -> bool
val nvals : t -> int
val print_type : type_t -> unit
val print_all : t -> unit
val print : t -> unit
val print_if_local : t -> unit
val print_name_space : t -> unit
val print_name : t -> unit
