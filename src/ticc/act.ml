(** $Id: act.ml,v 1.24 2005/04/30 18:25:30 luca Exp $ *)

(** For homogeneity, we have the concept of an action both 
    for a global, and for a local, action. *)
type act_scope_t = Global of Var.t list | Local 

(** This is the type of an action *) 
type t = {
  (** Name of the global action *)
  name: string;
  (** Scope of the action.  This contains also 
      the list of variables of the global action. 
      This could also be a hashtable, but it's 
      probably not worth it. *)
  scope: act_scope_t; 
}

exception Act_LocalActionsHaveNoVars
 
(** Create a global action with variables *)
let mk_global name vl = 
{
  name = name;
  scope = Global (vl); 
}

(** Create a local action (without variables) *)
let mk_local name = 
{
  name = name;
  scope = Local; 
}


(** Gets the name *)
let get_name a = a.name 

(** Gets the variables *)
let get_vars a = 
  match a.scope with 
    Local -> raise Act_LocalActionsHaveNoVars
  | Global (vl) -> vl 

(** Prints full information on an action *)
let print_full a = 
  print_string a.name; 
  match a.scope with
    Global (vl) -> 
      print_string "\n\tGlobal"; 
      List.iter Var.print_name_space vl
  | Local  -> 
      print_string "\n\tLocal"

(** Prints an action declaration *) 
let print a = 
  Printf.printf "\nact %s: " a.name; 
  begin
  match a.scope with 
    Global (vl) -> 
      print_string (String.concat " ," (List.map Var.get_name vl))
  | Local -> ()
  end;
  print_string ";"

