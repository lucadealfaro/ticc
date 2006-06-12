(** This is the main module of ticc, defining all the functions that
    are available to the user at the toplevel. The code of these 
    functions is also documented                                   *)

(** {2 Types} *) 

(** This is the type of a symbolic representation of a set of
    states.  MDDs are used, but you don't need to know the details. *)
type state_set_t

(** This is the type of a symbolic module. *)
type symbolic_module_t

(** "Enumerative modules" do not have a type: they are referred by
    their name (a string).  When you mention the string, TICC will
    look the string up in a symbol table. *) 

(* **************************************************************** *)

(** {2 Symbolic Module Operations} 
    These functions are used to compose and validate modules.    
*) 

(** Indicates that the operation has not yet been implemented for
    timed modules. *) 
exception NoTimedSupport

(** [compose m1 m2 m]  composes the symbolic modules [m1] and [m2], 
    and leaves the result in [m]. 
    The function takes a first optional parameter, which is the name
    of the result.  If no name is provided, then a name is generated
    from the names of m1 and m2, separated by '*'.  *)
val compose : ?result_name:string -> symbolic_module_t -> symbolic_module_t -> symbolic_module_t

val compose_alt : ?result_name:string -> symbolic_module_t -> symbolic_module_t -> symbolic_module_t

(*
(** [compose2opt ml1 ml2 l]  composes the list of symbolic modules 
    [ml1] and [m2], and leaves the result in [l]. 
    The function takes a first optional parameter, which is the name
    of the result.  If no name is provided, then a name is generated
    from the names of m1 and m2, separated by '*'.  *)
val compose2opt : ?result_name:string -> symbolic_module_t list -> symbolic_module_t list -> symbolic_module_t list

(** [compose2pes ml1 ml2 l]  composes the list of symbolic modules 
    [ml1] and [m2], and leaves the result in [l]. 
    The function takes a first optional parameter, which is the name
    of the result.  If no name is provided, then a name is generated
    from the names of m1 and m2, separated by '*'.  *)
val compose2pes : ?result_name:string -> symbolic_module_t list -> symbolic_module_t list -> symbolic_module_t list

(** calculate an input invariant based on pairwise composition *)
val calculate_inv : symbolic_module_t list -> state_set_t
 *)

(** This exception can be thrown by [compose] to indicate that the
    symbolic modules are not composable. *)
exception Modules_not_composable 

(** this exception can be thrown by [compose] to indicate that the
    symbolic modules being composed are incompatible. *)
exception Incompatible_Modules

(** [lo_post m set] computes the Post of [set], in symbolic module [m],
    with respect to local and output actions. *)
val lo_post : symbolic_module_t -> state_set_t -> state_set_t

(** [i_post m set] compustes the Post of [set], in the symbolic module
    [m], with respect to input actions. *)
val i_post : symbolic_module_t -> state_set_t -> state_set_t

(** [win_i_safe m set] computes the set of states of the symbolic
    module [m] from which Input can win the safety game with goal of
    staying forever in the set [set]. *)
val win_i_safe : symbolic_module_t -> state_set_t -> state_set_t
    
(** [win_lo_safe m set] computes the set of states of the symbolic
    module [m] from which Output can win the safety game with goal of
    staying forever in the set [set]. *)
val win_lo_safe : symbolic_module_t -> state_set_t -> state_set_t

(** [get_iinv m] returns the MDD representing the input invariant of
    module [m] *)
val get_iinv : symbolic_module_t -> state_set_t

(** [get_oinv m] returns the MDD representing the output invariant of
    module [m] *)
val get_oinv : symbolic_module_t -> state_set_t

(* **************************************************************** *)

(** {2 Parsing / Input functions}
    These functions are used to read text input, and produce internal
    representations. *)

(** Parses a file, given the file name. The file contains the
    declarations of the variables and the modules. 
    When parse aborts because of an error, all the parsed stuff is
    lost, so that you can try to parse the file again after correcting
    the problem. *)
val parse : string -> unit

(** Given the module name, it builds the symbolic representation of a
    module, and returns the symbolic module. *)
val mk_sym : string -> symbolic_module_t

(** Given the name of the stateset, it build the symbolic
    representation of the stateset (an MDD), and returns it. *)
val mk_set : string -> state_set_t

(** Given a string containing an expression, returns the symbolic
    representation of the set of states that satisfy the
    expression. *)
val parse_stateset : string -> state_set_t

(** [clone mod_name1 mod_name2] clones a parsed (not symbolic)
    module with name [mod_name1], producing a parsed module
    [mod_name2].  *)
val clone : string -> string -> unit

(** [sym_clone m] returns a clone of the symbolic module m *)
val sym_clone : Symmod.t -> Symmod.t 

(* **************************************************************** *)

(** {2 Stateset Operations} *) 

(** [set_equal set1 set2] returns a boolean indicating whether the
    symbolic sets [set1] and [set2] are equal or not. *) 
val set_equal : state_set_t -> state_set_t -> bool

(** [set_is_subset set1 set2] returns a boolean indicating whether the 
    symbolic set [set1] is a subset of [set2]. *) 
val set_is_subset : state_set_t -> state_set_t -> bool

(** [set_is_empty set] returns a boolean indicating whether the
    symbolic set [set] is empty. *) 
val set_is_empty : state_set_t -> bool 

(** Logical and of statesets *)
val set_and : state_set_t -> state_set_t -> state_set_t

(** Logical or of statesets *) 
val set_or : state_set_t -> state_set_t -> state_set_t

(** Logical implication of statesets *) 
val set_impl : state_set_t -> state_set_t -> state_set_t

(** Logical not of statesets *) 
val set_not : state_set_t -> state_set_t

(* **************************************************************** *)

(** {2 Simulation} *) 

(** [simulate sm expr n file_name] random simulates the module
    [sm], starting from states satisfying the Ticc expression [expr],
    for [n] steps.  The result is saved in the HTML file
    [file_name]. If [expr] is empty (that is, ""), the initial
    condition of [sm] is used. *)
val simulate : symbolic_module_t -> string -> int -> string -> unit

(* **************************************************************** *)

(** {2 Printing/Output Functions} *) 

(** Prints the (enumerative) top level, i.e., all the modules,
    variables, and sets that have been parsed. *)
val print_toplevel : unit -> unit

(** Prints debugging information about the (enumerative) top level,
    i.e., all the modules, variables, and sets that have been
    parsed. *)
val print_debug_toplevel : unit -> unit

(** [print_stateset set] prints the MDD representation of the set
    [set].  Note: this is going to be readable only for simple sets! *)
val print_stateset : state_set_t -> unit

(** Prints the list of modules that have been parsed so far.  This
    does NOT include the symbolic modules, such as those that have
    been created via operations. *) 
val print_list_modules : unit -> unit

(** Prints the list of symbolic modules *)
val print_list_symmodules : unit -> unit

(** Prints all the global variables *)
val print_vars : unit -> unit

(** [print_symmod m] 
    prints the symbolic module [m]. *) 
val print_symmod : symbolic_module_t -> unit

(** [print_symmod_iinv m] 
    prints the input invariant of module [m]. *) 
val print_symmod_iinv : symbolic_module_t -> unit

(** [print_symmod_oinv m] 
    prints the output invariant of module [m]. *) 
val print_symmod_oinv : symbolic_module_t -> unit

(** [print_input_restriction m action_name] prints the following. 
    This function can be used to find out how composition restricted
    the availability of an input action.
    Let phi be the input invariant of the module [m], tr be the 
    input transition relation associated with [action_name], and let
    lv be the set of local variables of [m]. 
    Then, the function prints: 
    \exists lv' . (tr /\ phi /\ not phi')
    Rationale: tr /\ phi /\ not phi' are the input state-changes
    associated with [action_name] that break the input invariant. 
    We quantify away the primed local variables, because their update
    is deterministic, so there is nothing to be gained by writing out
    their updated value. 
 *)
val print_input_restriction : symbolic_module_t -> string -> unit

(** [print_module_full m]
    Prints full information about a parsed module named [m]. *)
val print_module_full : string -> unit

(** [print_symmod_rules m act]
    Prints the symbolic transition relations for the action [act] of
    the symbolic module [m]. *)
val print_symmod_rules : symbolic_module_t -> string -> unit

(** [print_bool b] 
    Prints 'true' if [b] is true, and prints 'false' otherwise. *)
val print_bool : bool -> unit
