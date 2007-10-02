exception TypeAssertFailed

(** This is the type of the variable content *)
type type_t = 
    Bool 
  | Range of int * int 
  | Clock of int ref

let clock_type = Clock (ref (-1))
let bool_type = Bool
let range_type a b = Range (a, b)


(** This is the type of a variable *)   
type t = {
  (** name of the variable *)
  name  : string;
  (** type of the variable *) 
  vtype  : type_t;
  (** name of the module where it appears.  If None, then it is a
      program variable, and not a local variable of a module. *) 
  mod_name: string option; 
}

(** Creates a fresh variable *) 
let mk n typ mn: t =
  (* clock variables should get their own instance of
     the type, because the type contains a mutable integer
     which is specific to each variable.

     A clock which is declared but not compared with any constant
     keeps its "-1" bound. Thus, it gives rise to a symbolic
     variable with only one value, which is rightfully ignored 
     by the mdd manager. *)
  match typ with
    Clock _ -> {name = n; vtype = Clock (ref (-1)); mod_name = mn}
  | _ -> {name = n; vtype = typ; mod_name = mn}

(** Creates a fresh variable, whose type is the same of [v] *) 
let mk_same_type (v: t) n mn : t =
  (* clock variables should get their own instance of
     the type, because the type contains a mutable integer
     which is specific to each variable. *)
  match v.vtype with
    Clock cx -> {name = n; vtype = Clock (ref !cx); mod_name = mn}
  | _ -> {name = n; vtype = v.vtype; mod_name = mn}


(** Raises exception if [t] is not an integer. *)
let assert_int (v: t) : unit =
  match v.vtype with
    Bool -> raise TypeAssertFailed
  | _ -> ()

(** Raises exception if [t] is not a boolean. *)
let assert_bool (v: t) : unit =
  match v.vtype with
    Bool -> ()
  | _ -> raise TypeAssertFailed

(** Access functions: *)

(** gets the name *)
let get_name (v : t) : string = v.name 

(** Gets the clock bound *)
let get_bound (v: t) : int = 
  match v.vtype with 
    Clock mv -> !mv
  | _ -> raise TypeAssertFailed

(** Sets the clock bound *)
let set_bound v n : unit =
  match v.vtype with 
    Clock mv -> mv := n
  | _ -> raise TypeAssertFailed 

(** gets the type *)
let get_type (v : t) = v.vtype 

(** gets the module name *)
let get_mod_name v = v.mod_name

(** test: is it a global var? *)
let is_global v : bool = 
  match v.mod_name with 
    None   -> true
  | Some _ -> false

(** test: is it a clock? *) 
let is_clock v : bool = 
  match v.vtype with 
    Clock _ -> true
  | _       -> false

(** test: is it boolean? *) 
let is_bool v : bool = 
  match v.vtype with 
    Bool -> true
  | _    -> false

(** test: is it a range? *) 
let is_range v : bool = 
  match v.vtype with 
    Range _ -> true
  | _       -> false

(** Number of values a variable can assume *)
let nvals v : int = 
  match v.vtype with 
    Bool -> 2
  | Range (_,n) -> n + 1
      (* a clock needs to go from 0 to (maximum constant + 1) *)
  | Clock b -> !b + 2


(** Print functions: *)

(** Prints a variable type *)
let print_type = function 
    Bool -> Printf.printf "bool"
  | Range (i, j) -> 
        Printf.printf "[%d..%d]" i j
  | Clock mv -> 
      Printf.printf "clock // bound = %d" !mv

(** Print full details for variable *)
let print_all (v : t) =
  Printf.printf "\n\t%s: " v.name ;
  print_type v.vtype; 
  let prv = function 
      None -> Printf.printf ", global variable"
    | Some(m) -> Printf.printf ", local to module %s" m
  in prv v.mod_name

(** Print a variable declaration *) 
let print (v : t) = 
  begin
    match v.mod_name with 
      None -> 
	(* it's a global variable *)
	Printf.printf "\nvar %s: " v.name
    | Some(m) -> 
	Printf.printf "\n  var %s: " v.name
  end; 
  print_type v.vtype;
  Printf.printf ";"

(** Print a variable declaration *) 
let print_if_local (v : t) = 
  match v.mod_name with 
    None -> ()
  | Some(m) -> 
      begin
	Printf.printf "\n  var %s: " v.name; 
	print_type v.vtype;
	Printf.printf ";"
      end
	
(** Print a variable name with trailing space *)
let print_name_space (v: t) = Printf.printf "%s " v.name

(** Print a variable name *)
let print_name (v: t) = Printf.printf "%s" v.name
