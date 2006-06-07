(** $Id: ast.ml,v 1.31 2005/12/29 17:50:03 mfaella Exp $ *)

type pos = Lexing.position 

type binop = Eq | Ne | Gt | Lt | Ge | Le | And | Or | Implies 
             | Plus | Minus | Mod

type unop = Not | Nondet
 
(** This is the type of an ast.  The field [pos] indicates the
    position of the expression in the source file, and it is used for
    error reporting *)
type t  = Int   of int * pos
        | Tval  of bool * pos     (* truth value *) 
        | Binop of binop * t * t * pos
        | Unop  of unop * t * pos
	| Variable of Var.t * bool * pos  (* the bool keeps track of
						 whether it's primed *) 

(** This function applies a function to all variables of an
    expression. The parameter [primedness] allows to apply the
    function to primed/unprimed variables only.
    Input: 
    e: the expression
    f: Var.t -> Lexing.pos -> unit, the function applied to primed vars. *)    
let iter_vars_generic e f (primedness: bool option) = 
  let rec cv = function 
      Variable (v, p, pos) -> begin
	match primedness with
	  None -> f v pos
	| Some x -> if p = x then f v pos
      end
	
    | Binop (_,e1,e2,_) -> cv e1; cv e2
    | Unop (_,e1,_) -> cv e1
    | _ -> ()
  in cv e


(** This function applies a function to all primed variables of an
    expression.
    Input: 
    e: the expression
    f: Var.t -> Lexing.pos -> unit, the function applied to primed vars. *)    
let iter_primed_vars e f = iter_vars_generic e f (Some true)

(** This function applies a function to all unprimed variables of an
    expression.
    Input: 
    e: the expression
    f: Var.t -> Lexing.pos -> unit, the function applied to unprimed
    vars. *)   
let iter_unprimed_vars e f = iter_vars_generic e f (Some false)


(** This function applies a function to all variables of an
    expression. *)
let iter_vars e f = iter_vars_generic e f None

(** This function replaces each variable [v] in an expression
  with the variable [f v], and returns the new expression.
    Input: 
    e: the expression
    f: Var.t -> Var.t, the function applied to variables. *)    
let rec replace_vars (e: t) (f: Var.t -> Var.t): t = 
  match e with
    Variable (v, pr, pos) -> Variable (f v, pr, pos)
  | Binop (x,e1,e2,y) -> 
      let t1 = replace_vars e1 f 
      and t2 = replace_vars e2 f in
      Binop (x,t1,t2,y)
  | Unop (x,e1,y) -> Unop (x, replace_vars e1 f, y)
  | x -> x


(** returns a list of all primed variables in e.  Duplicates are NOT
    removed. *)
let get_primed_vars e : Var.t list =
  let collection = ref [] in
  let collect_one v pos : unit =
    collection := v::!collection
  in
  iter_primed_vars e collect_one;
  !collection

(** Returns a hsetmap of all primed variables in e.  
    Duplicates are removed. 
    The variables are returned as variables, and so, implicitly,
    unprimed. *)
let get_primed_vars_hsetmap e : (string, Var.t) Hsetmap.t =
  let l = get_primed_vars e in
  let h = Hsetmap.mk () in
  let add_one v =
    let name = Var.get_name v in
    Hsetmap.add h name v
  in
  List.iter add_one l;
  h

(** Returns a list of all unprimed variables in e.  Duplicates are NOT
    removed. *)
let get_unprimed_vars e : Var.t list =
  let collection = ref [] in
  let collect_one v pos : unit =
    collection := v::!collection
  in
  iter_unprimed_vars e collect_one;
  !collection
    

(** Returns a list of all variables in e.  Duplicates are NOT
    removed. *)
let get_vars e : Var.t list =
  let collection = ref [] in
  let collect_one v pos : unit =
    collection := v::!collection
  in
  iter_vars e collect_one;
  !collection


(** Gets the position (in the source file) of an expression *)
let get_pos (e : t) : pos = 
  match e with
    Int(_,pos) -> pos
  | Tval(_,pos) -> pos
  | Binop(_,_,_,pos) -> pos
  | Unop(_,_,pos) -> pos 
  | Variable(_,_,pos) -> pos 
	
(** Prints a position *)
let print_pos (pos: pos) = 
  Printf.printf "\nLine %d col %d" 
    pos.Lexing.pos_lnum 
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

(** Prints the position of an expression *)
let print_epos (e: t) = 
  print_pos (get_pos e)
    
(** Prints an expression in a way that can be parsed *)
let rec print (e : t) =
  match e with
    Int(i,pos) -> print_int i; (*print_pos pos; print_newline () *)
  | Tval(b,pos) ->
      let v = if b 
      then "true"
      else "false"
      in print_string v; 
      (*print_pos pos; print_newline () *)
  | Binop(op,lexp,rexp,pos) ->
      print_string "("; print lexp;
      let v = match op with 
	  Eq  ->  " = "
	| Ne  ->  " != "
	| Gt  ->  " > "
	| Lt  ->  " < "
	| Ge  ->  " => "
	| Le  ->  " =< "
	| And ->  " & "
	| Or  ->  " | "
	| Implies ->  " -> "
	    (* | Equiv ->  " == " *)
	| Plus  ->  " + " 
	| Minus ->  " - " 
	| Mod   -> " mod "
      in print_string v; 
      print rexp;
      print_string ")"; 
    | Unop(op,uexp,pos) -> 
	print_string "("; 
	let v = match op with
            Not -> " ~"
	  | Nondet -> " nondet"
	in print_string v; 
	print uexp; 
	print_string ")"
    | Variable (v, p, pos) -> 
	Var.print_name v; 
	if p then print_string "'"

(** This function prints an assignment, which is an expression of 
    the form v := e. 
    The special rule is needed to print the := token. *)
let print_asg = function 
    Binop (Eq, Variable (v,p,posv), e, pos) -> 
      print (Variable (v,p,posv));
      print_string " := ";
      print e
  | f -> 
      print_string " [Error: this should have been an assignment!] ";
      print f
