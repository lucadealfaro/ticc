(** $Id: var.ml,v 1.25 2005/09/01 22:12:51 luca Exp $ *)

exception TypeAssertFailed

(** This is the type of a clock bound *)
type clock_bound_t = {mutable maxval: int}

(** This is the type of the variable content *)
type data_t = 
    Bool 
  | Range of int * int 
  | Clock of clock_bound_t

(** Gets the clock bound *)
let get_bound b = b.maxval

(** Sets the clock bound *)
let set_bound b n = b.maxval <- n 

(** Raises exception if [t] is not an integer type. *)
let assert_int (t: data_t) : unit =
  match t with
    Bool -> raise TypeAssertFailed
  | _ -> ()

(** Raises exception if [t] is not a boolean type. *)
let assert_bool (t: data_t) : unit =
  match t with
    Bool -> ()
  | _ -> raise TypeAssertFailed

(** This is the type of a variable *)   
type t = {
  (** name of the variable *)
  name  : string;
  (** type of the variable *) 
  vtype  : data_t;
  (** name of the module where it appears.  If None, then it is a
      program variable, and not a local variable of a module. *) 
  mod_name: string option; 
}

(** Creates a fresh variable *) 
let mk n typ mn: t = 
  {name = n; vtype = typ; mod_name = mn}


(** Access functions: *)

(** gets the name *)
let get_name (v : t) : string = v.name 

(** gets the type *)
let get_type (v : t) = v.vtype 

(** gets the module name *)
let get_mod_name v = v.mod_name

(** test: is it a global var? *)
let is_global v : bool = 
  match v.mod_name with 
    None   -> true
  | Some _ -> false

(** test: it is a clock? *) 
let is_clock v : bool = 
  match v.vtype with 
    Clock _ -> true
  | _       -> false

(** Number of values a variable can assume *)
let nvals v = 
  match v.vtype with 
    Bool -> 2
  | Range (_,n) -> n+1 
  | Clock (b) -> b.maxval 


(** Print functions: *)

(** Prints a variable type *)
let print_type = function 
    Bool -> Printf.printf "bool"
  | Range (i, j) -> 
        Printf.printf "[%d..%d]" i j
  | Clock(mv) -> 
      Printf.printf "clock // bound = %d" mv.maxval

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
