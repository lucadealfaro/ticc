(** This is the main module of ticc, defining all the functions that
    are available to the user at the toplevel. The code of these 
    functions is also documented                                   *)

(** {2 Types} *) 

(** This is the type of a symbolic representation of a set of
    states.  MDDs are used, but you don't need to know the details. *)
(** Actually, this type is temporarily exposed, so that the user
    (or rather, the developer) can directly call all sort of internal 
    unsupported functions :-) *)  
type stateset_t = Mlglu.mdd

(** This is the type of a symbolic module. *)
type symbolic_module_t = Symmod.t

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

(** This exception is thrown by [compose] if 
    the symbolic modules being composed are not composable. *)
exception Modules_not_composable 

(** This exception is thrown by [compose] if
    the symbolic modules being composed are incompatible. *)
exception Incompatible_Modules

(** [compose m1 m2 m]  composes the symbolic modules [m1] and [m2], 
    and leaves the result in [m]. 
    The function takes a first optional parameter, which is the name
    of the result.  If no name is provided, then a name is generated
    from the names of m1 and m2, separated by '*'.  *)
val compose : ?result_name:string -> symbolic_module_t -> symbolic_module_t -> symbolic_module_t

(** [win_i_safe m set] computes the set of states of the symbolic
    module [m] from which Input can win the safety game with goal of
    staying forever in the set [set]. *)
val win_i_safe : symbolic_module_t -> stateset_t -> stateset_t
    
(** [win_lo_safe m set] computes the set of states of the symbolic
    module [m] from which Output can win the safety game with goal of
    staying forever in the set [set]. *)
val win_lo_safe : symbolic_module_t -> stateset_t -> stateset_t

(** [get_iinv m] returns the MDD representing the input invariant of
    module [m] *)
val get_iinv : symbolic_module_t -> stateset_t

(** [get_oinv m] returns the MDD representing the output invariant of
    module [m] *)
val get_oinv : symbolic_module_t -> stateset_t

(** [get_initial m] return the MDD representing the set of initial states
    of module [m] *)
val get_initial : symbolic_module_t -> stateset_t

(** [set_initial m phi] replaces the initial condition of the 
    symbolic module [m] with [phi], and erases the cached set of 
    reachable states for [m]. *)
val set_initial : symbolic_module_t -> stateset_t -> unit

(** [get_stateset m sn] returns the MDD representing the stateset
    (set of states) with name [sn] declared in module [m]. *)
val get_stateset : symbolic_module_t -> string -> stateset_t

(** [get_reachable m] returns the MDD representing the reachable states of 
    module [m].  The answer is cached, so it does not cost more to ask 
    multiple times. *)
val get_reachable : symbolic_module_t -> stateset_t

(** [get_input_restriction m r] returns the way in which it is not
    possible to use rule [r] in module [m], due to the input
    invariant. *)
val get_input_restriction : symbolic_module_t -> string -> stateset_t

(** [close m a] closes the symbolic module [m] with respect to input
    action [a], so that [a] is no longer accepted as input. *)
val close : symbolic_module_t -> string -> symbolic_module_t

(** [forget m] forgets all the MDDs associated with the module [m], so 
    that the garbage collector can collect them.  Useful for getting
    rid of large intermediate results.  The effect of calling
    subsequent functions on [m] is undefined!  Use with care! *)
val forget : symbolic_module_t -> unit

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
val mk_set : string -> stateset_t

(** Given a string containing an expression, returns the symbolic
    representation of the set of states that satisfy the
    expression. *)
val parse_stateset : string -> stateset_t

(** [clone mod_name1 mod_name2] clones a parsed (not symbolic)
    module with name [mod_name1], producing a parsed module
    [mod_name2].  *)
val clone : string -> string -> unit

(** [sym_clone m] returns a clone of the symbolic module m *)
val sym_clone : symbolic_module_t -> symbolic_module_t

(* **************************************************************** *)

(** {2 Stateset Operations} *) 

(** [set_equal set1 set2] returns a boolean indicating whether the
    symbolic sets [set1] and [set2] are equal or not. *) 
val set_equal : stateset_t -> stateset_t -> bool

(** [set_is_subset set1 set2] returns a boolean indicating whether the 
    symbolic set [set1] is a subset of [set2]. *) 
val set_is_subset : stateset_t -> stateset_t -> bool

(** [set_is_empty set] returns a boolean indicating whether the
    symbolic set [set] is empty. *) 
val set_is_empty : stateset_t -> bool 

(** [set_is_nonempty set] returns a boolean indicating whether the
    symbolic set [set] is nonempty. *) 
val set_is_nonempty : stateset_t -> bool 

(** Logical and of statesets *)
val set_and : stateset_t -> stateset_t -> stateset_t

(** Logical or of statesets *) 
val set_or : stateset_t -> stateset_t -> stateset_t

(** Logical implication of statesets *) 
val set_impl : stateset_t -> stateset_t -> stateset_t

(** Logical not of statesets *) 
val set_not : stateset_t -> stateset_t

(* **************************************************************** *)

(** {2 Simulation} *) 

(** [simulate sm expr n file_name] random simulates the module
    [sm], starting from states satisfying the Ticc expression [expr],
    for [n] steps.  The result is saved in the HTML file
    [file_name]. If [expr] is empty (that is, ""), the initial
    condition of [sm] is used. *)
val simulate : symbolic_module_t -> string -> int -> string -> unit

(* **************************************************************** *)

(** {2 Refinement} *) 

(** [refinement m1 m2]
    Returns the refinement relation between [m1] and [m2]. *)
val refinement : symbolic_module_t -> symbolic_module_t -> stateset_t

(** [refines m1 m2]
    Checks whether symbolic module [m1] refines symbolic module [m2]. *)
val refines : symbolic_module_t -> symbolic_module_t -> bool

(* **************************************************************** *)

(** {2 CTL Operators} *) 

(** {3 Existential Operators} *)

(** [ctl_e_until sm phi gamma] computes \exists ([phi] Until [gamma]) for module [sm] *)
val ctl_e_until: symbolic_module_t -> stateset_t -> stateset_t -> stateset_t

(** [ctl_e_waitfor sm phi gamma] computes \exists ([phi] Waitfor [gamma]) for module [sm] *)
val ctl_e_until: symbolic_module_t -> stateset_t -> stateset_t -> stateset_t

(** [ctl_e_f sm phi] computes \exists F [phi] for module [sm] *)
val ctl_e_f: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_e_g sm phi] computes \exists G [phi] for module [sm] *)
val ctl_e_g: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_e_next sm phi] computes \exists X [phi] for module [sm] *)
val ctl_e_next: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_e_input_next sm phi] computes \exists X^I [phi] for module [sm], 
    where X^I is the next operator when only input actions can be taken. *)
val ctl_e_input_next: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_e_output_next sm phi] computes \exists X^O [phi] for module [sm], 
    where X^O is the next operator when only output actions can be taken. *)
val ctl_e_output_next: symbolic_module_t -> stateset_t -> stateset_t

(** {3 Universal Operators} *)

(** [ctl_a_until sm phi gamma] computes \forall ([phi] Until [gamma]) for module [sm] *)
val ctl_a_until: symbolic_module_t -> stateset_t -> stateset_t -> stateset_t

(** [ctl_a_waitfor sm phi gamma] computes \forall ([phi] Waitfor [gamma]) for module [sm] *)
val ctl_a_waitfor: symbolic_module_t -> stateset_t -> stateset_t -> stateset_t

(** [ctl_a_f sm phi] computes \forall F [phi] for module [sm] *)
val ctl_a_f: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_a_g sm phi] computes \forall G [phi] for module [sm] *)
val ctl_a_g: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_a_next sm phi] computes \forall X [phi] for module [sm] *)
val ctl_a_next: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_a_input_next sm phi] computes \forall X^I [phi] for module [sm], 
    where X^I is the next operator when only input actions can be taken. *)
val ctl_a_input_next: symbolic_module_t -> stateset_t -> stateset_t

(** [ctl_a_output_next sm phi] computes \forall X^O [phi] for module [sm], 
    where X^O is the next operator when only output actions can be taken. *)
val ctl_a_output_next: symbolic_module_t -> stateset_t -> stateset_t

(** {3 Logical Operators} *)

(** These are the same as [set_and], [set_or]. *)
val ctl_and: stateset_t -> stateset_t -> stateset_t
val ctl_or: stateset_t -> stateset_t -> stateset_t

(** This is different from [set_not].  [set_not] simply complements a set. 
    [ctl_not] complements the set, then conjoins the complement with the 
    input and the output invariant.  Thus, [ctl_not], like the other CTL 
    operators, returns only states that satisfy both invariants. *)
val ctl_not: symbolic_module_t -> stateset_t -> stateset_t

(* **************************************************************** *)

(** {2 Post Operators} *) 

(** [lo_post m set] computes the Post of [set], in symbolic module [m],
    with respect to local and output actions. *)
val lo_post : symbolic_module_t -> stateset_t -> stateset_t

(** [i_post m set] compustes the Post of [set], in the symbolic module
    [m], with respect to input actions. *)
val i_post : symbolic_module_t -> stateset_t -> stateset_t

(** [post m set] compustes the Post of [set], in the symbolic module
    [m], with respect to input, output, and local actions. *)
val post : symbolic_module_t -> stateset_t -> stateset_t

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
val print_stateset : stateset_t -> unit

(** Prints the list of modules that have been parsed so far.  This
    does NOT include the symbolic modules, such as those that have
    been created via operations. *) 
val print_list_modules : unit -> unit

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

(** [print_restr_paths sm (r: string) (n_traces: int)]
    prints [n_traces] restriction paths for rule [r] of module [sm],
    leading from the initial condition, to an istance of input rule
    [r], to the set of bad states of [sm]. *)
val print_restr_paths : symbolic_module_t -> string -> int -> unit

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

(** {2 Operations on timed modules} *)

(** [i_live sm] returns the set of I-live states of module [sm].
  If the optional labelled argument [verbose] is set to true,
  the function provides a more detailed progress indicator. *)
val i_live     : ?verbose:bool -> symbolic_module_t -> stateset_t

(** [o_live sm] returns the set of O-live states of module [sm].
  If the optional labelled argument [verbose] is set to true,
  the function provides a more detailed progress indicator. *)
val o_live     : ?verbose:bool -> symbolic_module_t -> stateset_t

(** [i_live_alt sm] returns the set of I-live states of module [sm],
  using an alternative algorithm (Jurdzinski's progress measure
  algorithm). *)
val i_live_alt :                  symbolic_module_t -> stateset_t

(** {2 Experimental or debugging features} *)

val unwrap_stateset : stateset_t -> Mlglu.mdd 
val wrap_stateset   : Mlglu.mdd  -> stateset_t

