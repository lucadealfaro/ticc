/* The ticc parser */ 

 %{
   open Ast
     
   exception Internal_error
     
   exception ParseError 

   let curr_mod: (Mod.t option) ref = ref None  (* Current module *)

   (** prints a parse error *)
   let parse_error s : unit = 
     let line = Ticlex.curr_pos.Ticlex.lex_line in 
     let col  = Ticlex.curr_pos.Ticlex.lex_col in 
     let tok  = Ticlex.curr_pos.Ticlex.tok in 
     Printf.printf "Line %d Col %d: %s: '%s'\n" line col s tok;
     (* reset parsing *)
     curr_mod := None;
     Prog.cleanup Prog.parselevel

   (** This function looks up a global variable both in the parselevel
     and in the toplevel.
     Raises Not_found if there is no global var called [name]. *)
   let lookup_global_var (name: string) : Var.t = 
     (* looks up at toplevel *)
     if Prog.is_var_name_def_top name
     then Prog.lookup_var_top name 
       (* else, looks up in the parselevel *)
     else if Prog.is_var_name_def Prog.parselevel name 
     then Prog.lookup_var Prog.parselevel name 
     else raise Not_found


   (** This function looks up a variable found in an expression,
     in a context-sensitive fashion. *)
   let lookup_exp_var (name: string) pos : Var.t = 
     try begin
       match !curr_mod with 
	 None -> (* the variable was found in a stateset definition *)
	   begin
	     let lookup_local_var name m (temp_var: Var.t option) : Var.t option =
	       match temp_var with
		 Some v -> Some v
	       | None ->
		   try Some (Mod.lookup_lvar m name)
		   with Not_found -> None
	     in
	     let v_top   = Prog.fold_mods Prog.toplevel 
	       (lookup_local_var name) None
	     and v_parse = Prog.fold_mods Prog.parselevel 
	       (lookup_local_var name) None in
	     match (v_top, v_parse) with
	       (Some v, _)    -> v
	     | (None, Some v) -> v
	     | (None, None) -> lookup_global_var name
	   end
       | Some m -> (* the variable was found in a rule body *)
	   let mod_name = Mod.get_name m in
	   (* the name of a local var is given by mod_name.var_name *)
	   let local_name = mod_name ^ "." ^ name in 
           if Mod.is_lvar_name_def m local_name 
           then Mod.lookup_lvar m local_name 
           else (* it's not local, see if it's a global var *)
	   lookup_global_var name 
     end
     with Not_found ->
       Printf.printf "Line %d Col %d: Error: variable %s undefined\n" 
       pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
       raise ParseError 
          

   (** declares a list of stateless variables *)
   let declare_stateless (vl: (string * Lexing.position) list) : unit =
     match !curr_mod with 
       None -> raise Internal_error
     | Some m -> 
	 let add_v (name, pos) : unit =
	   try
	     let v = lookup_global_var name in
	     Mod.add_fvar m v
	   with Not_found ->
	     Printf.printf "Line %d Col %d: Error: variable %s is not a known global variable\n" 
	     pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
             raise ParseError 
	 in
	 List.iter add_v vl


   (** declares a stateset *)
   let declare_stateset (name: string) (exp: Ast.t) : unit =
     match !curr_mod with 
       Some (m) -> 
	 (* The stateset is declared in module m *)
	 (* Checks whether there is already a stateset by the same name *)
	 if Mod.is_sset_name_def m name then begin 
	   let pos = Parsing.rhs_start_pos 1 in
	   Printf.printf "Line %d Col %d: Error: double definition for stateset %s\n" 
             pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
	   raise ParseError 
	 end;
	 (* The real name of the set is the name of the module "." the name
	    of the set. *) 
	 let mod_name = Mod.get_name m in
	 let local_name = mod_name ^ "." ^ name in 
	 Mod.add_sset m local_name exp 
     | None -> 
	 (* The stateset is declared at the top level *)
	 (* check if there is another stateset by the same name *)
	 if Prog.is_sset_name_def Prog.parselevel name or 
	   Prog.is_sset_name_def_top name then begin
	     (* stateset name already defined in toplevel or parselevel *)
	     let pos = Parsing.rhs_start_pos 1 in
	     Printf.printf "Line %d Col %d: Error: double definition for stateset %s\n" 
               pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
	     raise ParseError 
	   end;
	 Prog.add_sset Prog.parselevel name exp
       

   (** Declares global or local variable. 
     The decision is based on the scope. 
     Either global or inside current module (curr_mod) *)
   let declare_var (vl : (string * Lexing.position) list) (vt: Var.data_t) : unit = 
     let add_v (name, pos) = 
       match !curr_mod with
         Some (m) -> 
           (* I am in the scope of module m *) 
	   let mod_name = Mod.get_name m in
	   (* Creates the variable, with a name given by mod_name.var_name *)
	   let local_name = mod_name ^ "." ^ name in 
           if Mod.is_lvar_name_def m local_name 
           then begin
             Printf.printf "Line %d Col %d: Error: double definition of variable %s in module %s\n" 
               pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name (Mod.get_name m);
             raise ParseError 
           end else 
	     (* I also don't want local variables with the same name
		of global variables, to keep it simple.  Otherwise, if
		you declare a variable in the middle of a module,
		above it you refer to the global copy.  When the
		module is then printed, and you have the var
		declaration on top, things are not the same any more. *) 
	     if (Prog.is_var_name_def_top name) || 
	        (Prog.is_var_name_def Prog.parselevel name) then begin
		 Printf.printf "Line %d Col %d: Error: the local variable %s is already declared as global\n" 
		   pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
		 raise ParseError 
	       end else begin
		 let v = Var.mk local_name vt (Some mod_name) 
		   (* Adds the variable to the module *) 
		 in Mod.add_lvar m v
	       end
       | None -> 
           (* We are not in the scope of a module *) 
	   let newv = Var.mk name vt None in 
	   if Prog.is_var_name_def_top name then begin
	     (* If there is an identical variable at toplevel, 
		no need to redeclare it. *) 
	     let oldv = Prog.lookup_var_top name in 
	     if not (oldv = newv) then begin
	       Printf.printf "Line %d Col %d: Error: the variable %s exists already at toplevel, with different type\n" 
		 pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
	       raise ParseError 
	     end
	   end
	   else if Prog.is_var_name_def Prog.parselevel name 
           then begin
	     (* already defined in parselevel *) 
             Printf.printf "Line %d Col %d: Error: double definition of variable %s\n" 
               pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
             raise ParseError 
           end else begin
             (* Check that a global variable is not a clock *)
             match vt with 
               Var.Clock (_) ->
                 Printf.printf "Line %d Col %d: Error: clock %s is not in the scope of any module (clocks cannot be global)\n" 
                   pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) name;
                 raise ParseError 
             | Var.Range(_,_) | Var.Bool -> Prog.add_var Prog.parselevel newv
           end 
     in 
     (* declares all vars *) 
     List.iter add_v (List.rev vl)
       

   (** declares a module, given the name *)    
   let declare_mod (n: string) : Mod.t = 
     (* note that modules cannot be merged! *) 
     if (Prog.is_mod_name_def_top n) or (Prog.is_mod_name_def Prog.parselevel n)
     then begin
       let pos = Parsing.rhs_start_pos 1 in 
       Printf.printf "Line %d Col %d: Error: double definition of module %s\n" 
         pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) n;
       raise ParseError
     end else begin
       (* creates the module *)
       let new_mod = Mod.create_empty n in
       (* adds it to the program *) 
       Prog.add_mod Prog.parselevel new_mod; 
       (* enter module scope *)
       curr_mod := Some new_mod;
       new_mod
     end


   (** declares the rules (global and local) of an input action *)
   let declare_gina 
       (aname: string * Lexing.position) 
       (glrule: (Ast.t * Ast.t) list) 
       (locrule: (Ast.t * (Ast.t list)) list) : unit = 
     match !curr_mod with
       None ->
         Printf.printf "Input rules should belong to a module" ;
         raise Internal_error
     | Some m ->
         let (a_name, pos) = aname in 
         (* checks whether the module already has a rule for this action *)
         if Mod.is_irule_name_def m a_name
         then begin
           Printf.printf "Line %d Col %d: Error: an input rule for the global action %s is already defined in the module\n" 
             pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) a_name;
           raise ParseError 
         end else begin
           (* ok, we can add the rule to the module *)
           (* Note: here it is actually important to reverse the order
              of the lists of gcs, since the order, for deterministic 
              rules, is significant. *)
           let r = Rule.mk_irule a_name (List.rev glrule) (List.rev locrule) in 
           Mod.add_irule m r
         end


   (** declares the rule of an output action *)
   let declare_gouta (an: string * Lexing.position) (rl: (Ast.t * Ast.t) list) : unit = 
     match !curr_mod with
       None ->
         Printf.printf "Output rules should belong to a module" ;
         raise Internal_error
     | Some m ->
         let (a_name, pos) = an in 
         (* checks whether the module already has a rule for this action *)
         if Mod.is_orule_name_def m a_name
         then begin 
           Printf.printf "Line %d Col %d: Error: an output rule for the global action %s is already defined in the module\n" 
             pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) a_name;
           raise ParseError 
         end else begin
           (* ok, we can add the rule to the module *)
           let r = Rule.mk_orule a_name (List.rev rl) in 
           Mod.add_orule m r
         end
 

   let declare_lact (an: string * Lexing.position) (rl: (Ast.t * Ast.t) list) : unit = 
     match !curr_mod with
       None ->
         Printf.printf "Local rules should belong to a module" ;
         raise Internal_error
     | Some m ->
         let (a_name, pos) = an in 
         (* checks that no other rule with the same name exists in the module *) 
         if (Mod.is_irule_name_def m a_name) || (Mod.is_orule_name_def m a_name) || (Mod.is_lrule_name_def m a_name)
         then begin
           Printf.printf "Line %d Col %d: Error: the action %s is already defined\n" 
             pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) a_name;
           raise ParseError 
         end else begin 
           (* ok, we can add the rule to the module *)
           (* I reverse the list, as I want to keep the gc
              in the original order *)
	   (* Begin added by Leandro on August 30th, 2005 *)
	   let mod_name = Mod.get_name m in
	   (* the name of a local action is given by mod_name.lact_name *)
	   let local_name = mod_name ^ "." ^ a_name in
	   (* End added by Leandro *)
             let r = Rule.mk_lrule local_name (List.rev rl) in 
             Mod.add_lrule m r
         end

   (** makes a range type *)
   let make_range_type (lb: int) (ub: int) (pos: Lexing.position) : Var.data_t = 
     if lb != 0 then begin
       Printf.printf "Line %d Col %d: Error: ranges must start from 0\n" 
         pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
       raise ParseError 
     end else Var.Range(lb,ub)


   (** typechecks a module *)
   let type_check_module = function 
       None -> ()
     | Some (m) -> 
	 Typecheck.check_module m;
	 Typecheck.optimize_module m


 %}
 
%token <string> ID WILDID
%token <int> NUM
%token BOOL CLOCK TRUE FALSE 
%token VAR MODULE ENDMODULE
%token NONDET
%token OINV IINV STATELESS
%token INPUT OUTPUT GLOBAL LOCAL
%token BEGBLOCK ENDBLOCK
%token TO ELSE
%token EQ NE GT LT GE LE AND OR NOT IMPLIES 
%token PLUS MINUS MOD
%token COL SEMICOL LB RB LPAR RPAR DOTS COMMA COLEQ
%token PRIME EOF
%token STATESET INITIAL

     /** precedence table **/
 
%left AND OR IMPLIES 
%left NOT
%left EQ NE GT LT GE LE
%nonassoc MOD 
%left PLUS MINUS
 
%start design
%start stateset
 
%type <unit> design
%type <Ast.t> stateset

%% /* Grammar Rules and actions follow */

 design: varactmodlist { } ; 
 
 varactmodlist: | varactmod varactmodlist {}
                | EOF {} ;

 varactmod:    VAR varnamelist COL vartype           { declare_var $2 $4 }
             | MODULE modname COL moddecllist ENDMODULE 
 		 { type_check_module !curr_mod; 
		   curr_mod := None }
             | STATESET ID COL exp
                 { declare_stateset $2 $4};

 moddecllist:  vardecl    moddecllist { }
	     | invariant  moddecllist { }
	     | transition moddecllist { }
	     | stateless  moddecllist { }
	     | statesetd  moddecllist { }
             | initsetd   moddecllist { }
	     | {} ;

 varnamelist : varnamelist COMMA varname    {$3::$1} 
             | varname                      {[$1]} ;

 varname : ID {($1, Parsing.rhs_start_pos 1)} ; 

 vartype : BOOL                       { Var.Bool }
         | LB NUM DOTS NUM RB         { make_range_type $2 $4 (Parsing.rhs_start_pos 3) }
         | CLOCK                      { Var.Clock({Var.maxval = 0}) } ; 

 actname : ID                         { ($1, Parsing.rhs_start_pos 1)} ; 

 actexp  : ID                         { ($1, Parsing.rhs_start_pos 1)}
         | WILDID                     { ($1, Parsing.rhs_start_pos 1)} ; 

 modname : ID                         { declare_mod $1 };

 vardecl     : VAR varnamelist COL vartype           { declare_var $2 $4 };
 
 stateless   : STATELESS varnamelist           { declare_stateless $2 };

 statesetd   : STATESET ID COL exp { declare_stateset $2 $4 };

 initsetd    : INITIAL COL exp { declare_stateset Mod.init_sset_name $3 };

 invariant : OINV COL exp
               { match !curr_mod with
                   Some x -> Mod.add_oinv x $3
                 | None -> Printf.printf "Output invariants should belong to a module";
                   raise Internal_error}
           | IINV COL exp
               { match !curr_mod with
                   Some x -> Mod.add_iinv x $3
                 | None -> Printf.printf "Input invariants should belong to a module";
                   raise Internal_error} ;

 transition : INPUT  actexp  COL BEGBLOCK globalpart 
                                      localpart   ENDBLOCK { declare_gina  $2 $5 $6 }
            | OUTPUT actname COL BEGBLOCK guardedcmds ENDBLOCK { declare_gouta $2 $5 }
            | LOCAL  actname COL BEGBLOCK guardedcmds ENDBLOCK { declare_lact  $2 $5 } ; 

 globalpart : GLOBAL COL guardedcmds   {$3} 
            |                          {[]}

 localpart  : LOCAL COL detguardedcmds {$3}
	    |                          {[]}

 guardedcmds :                                 {[]}
             | guardedcmd SEMICOL guardedcmds  {$1::$3}
             | guardedcmd                      {[$1]} ;

 guardedcmd : exp TO optexp                    {($1,$3)} ;

 detguardedcmds :                                   {[]}
	        | detguardedcmd                     {[$1]}
                | detguardedcmds ELSE detguardedcmd {$3::$1} ;

 detguardedcmd : exp TO detcmdlist         {($1,$3)}
	       | exp TO detcmdlist SEMICOL {($1,$3)} ;

 detcmdlist :                                 { [] }  
            | detcmd                          { [$1] } 
            | detcmdlist COMMA detcmd         {$3::$1} ;

 detcmd : nextvar COLEQ exp          { Binop(Eq, $1, $3, Parsing.rhs_start_pos 2)} ;

 stateset : exp EOF {$1}

 optexp     :                        { Tval(true, Parsing.rhs_start_pos 1) }
	    | exp                    { $1 } ;

 exp        : NUM                    { Int($1, Parsing.rhs_start_pos 1)}
            | anyvar                 { $1 } 
            | TRUE                   { Tval(true, Parsing.rhs_start_pos 1)}
            | FALSE                  { Tval(false, Parsing.rhs_start_pos 1)} 
            | exp EQ exp             { Binop(Eq,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp NE exp             { Binop(Ne,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp GT exp             { Binop(Gt,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp LT exp             { Binop(Lt,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp GE exp             { Binop(Ge,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp LE exp             { Binop(Le,  $1, $3, Parsing.rhs_start_pos 2)} 
            | exp AND exp            { Binop(And, $1, $3, Parsing.rhs_start_pos 2)}
            | exp OR exp             { Binop(Or,  $1, $3, Parsing.rhs_start_pos 2)}   
            | exp IMPLIES exp        { Binop(Implies, $1, $3, Parsing.rhs_start_pos 2)}
            | NOT exp                { Unop(Not,    $2, Parsing.rhs_start_pos 1)}
	    | NONDET nextvar         { Unop(Nondet, $2, Parsing.rhs_start_pos 1)} 	
            | exp PLUS exp           { Binop(Plus,  $1, $3, Parsing.rhs_start_pos 2)}
            | exp MINUS exp          { Binop(Minus, $1, $3, Parsing.rhs_start_pos 2)}
            | exp MOD exp            { Binop(Mod,   $1, $3, Parsing.rhs_start_pos 2)}
            | LPAR exp RPAR          { $2 } ;
 		
 anyvar  :  var {$1} | nextvar {$1}		

 var     :  ID         { let pos = Parsing.rhs_start_pos 1 in 
                         let v   = lookup_exp_var $1 pos in 
                         Variable (v, false, pos) };

 nextvar :  ID PRIME   { let pos = Parsing.rhs_start_pos 1 in 
                         let v   = lookup_exp_var $1 pos in 
                         Variable (v, true, pos) };

%%

