(** This module contains the implementation of the simulate function and all
    of its support functions
*)

open Str;;
open Symmod;;

type stateset_t = Mlglu.mdd;;
type relationset_t = Mlglu.mdd;;

module VarSet = Vset.VS;;

type varid_t = Vset.varid_t;; 

open Ast;;

(** Random Simulation *)
exception User_Start_State_Violates_Invariants

(** The following is a random number generator. This code has been
    pinched from www.bagley.org/~doug/shootout
*)

let lastRef = ref 42;;

let generateRandom (range: int) : int =
  let random (max: int) =
    let im = 139968
    and ia = 3877
    and ic = 29573
    in
    let newLast = (!lastRef * ia + ic) mod im in
      lastRef := newLast;
      float_of_int max *. float_of_int newLast /. float im;
  in
  let rec loop (i: int) =
    let r = random 100 in
      if i > 1 then loop (i - 1) else r
  in
    (int_of_float(loop range) mod range)
;;

(** The following function computes the next stateset of a simulation cycle
    given a state. The algorithm,

    1. Iterate over all rules
    2. Apply post_rule on rule
    3. Find intersection of post_rule with input and output invariants
    3. if (MDD non-zero) Accumulate rule else discard rule
    5. Randomize over the accumulated rules and pick one rule
    6. post_rule for that rule and return result

*)

let computeNextState (sp: Symprog.t) (sm: Symmod.t) (set: stateset_t) :
    (stateset_t * Symmod.rule_t) =
  let mgr = Symprog.get_mgr sp in
  let zero = Mlglu.mdd_zero mgr in
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let rules = ref [] in
  let doOneRule (r: Symmod.rule_t) : unit =
    let dest = Ops.post_rule sp sm set r in
    let iDest = Mlglu.mdd_and dest iinv 1 1 in
    let oDest = Mlglu.mdd_and iDest oinv 1 1 in
      if not (Mlglu.mdd_equal oDest zero) then begin
	rules := r::!rules;
      end
  in
  let testRules : unit =
    Symmod.iter_irules sm doOneRule;
    Symmod.iter_lrules sm doOneRule;
    Symmod.iter_orules sm doOneRule;
  in
  let random = generateRandom (List.length !rules) in
    ((Ops.post_rule sp sm set (List.nth !rules random)), (List.nth !rules random));
;;

(** This function is used to parse a state string and return either the
    variables or their values in a list *)

let extractRow (state : string) (wantVars : int) (result : string list ref) : unit =
  let splitState = Str.split (Str.regexp " ") state in
  let rec extract (splitList : string list) : unit =
    match splitList with
	s::rest -> begin
	  let nameValue (s : string) = Str.split (Str.regexp "=") s in
	    match (nameValue s) with
		name::value ->
		  if (wantVars = 1) then begin
		    result := !result@[name]
		  end else begin
		    result := !result@value
		  end;
		  extract rest
	      | _ -> ();
	end
      | [] -> ()
  in
    extract splitState
;;

(** The following function is a wrapper around the Mlglu function to extract
    a cube. This is for convenience *)

let mdd_pick_one_cube (set : stateset_t) (vAll : VarSet.t) : stateset_t =
  let mgr = Mlglu.mdd_get_manager set in
    Mlglu.mdd_pick_one_cube mgr set vAll
;;

(** This function is used to create compact string representations of cubes.
    It takes a stateset and returns a string as a space separated set of
    name=value pairs. If the stateset corresponds to a cube, then the
    values of those variables that are free in the cube will be assigned
    '*' in this format. The parameter vAll is an optional parameter. If it
    is passed then variables in vAll that are not in the random cube will
    be part of the generated string. This way, assuming that vAll was used
    in picking the random cube, we will generate a string here that spans
    over all those variables *)

let mdd_cube_to_string (set : stateset_t) (vAll : VarSet.t option) :
    string =
  let compactCube = ref "" in
  let varsHash : (string, string) Hsetmap.t = Hsetmap.mk() in
  let mgr = Mlglu.mdd_get_manager set in
  let state = Mlglu.mdd_to_string mgr set in
  let splitState = Str.split (Str.regexp " ") state in
  let getValue (name : string) : string = Hsetmap.find varsHash name in
  let accumulate (name : string) (value : string) =
    if (value = "-") then
      compactCube := !compactCube^name^"=* "
    else
      compactCube := !compactCube^name^"="^value^" "
  in
  let initializeVarsHash vid : unit =
    let (v, p) = Symprog.get_var_p Symprog.toplevel vid in
    let varName = Var.get_name v in
      Hsetmap.add varsHash varName "-"
  in
  let rec extract (splitList : string list) : unit =
    match splitList with
	s::rest -> begin
	  let nameValue (s : string) = Str.split (Str.regexp "=") s in
	    match (nameValue s) with
		name::value::[] ->
		  if (Hsetmap.mem varsHash name) then begin
		    if ((getValue name) = "-") then
		      Hsetmap.modify varsHash name value
		    else
		      Hsetmap.modify varsHash name "*"
		  end;
		  extract rest;
	      | _ -> ();
	end
      | [] -> ()
  in
    begin
      match vAll with
	  Some(varset) ->
	    VarSet.iter initializeVarsHash varset;
	| None -> ()
    end;
    extract splitState;
    Hsetmap.iter accumulate varsHash;
    !compactCube
;;

(** The following function is used to print values in a table row *)

let printHtmlValues (n : int) (p : string list) (v : string list)
    (outChannel: out_channel) : unit =
  let rec recPrintHtmlValues (n : int) (p : string list) (v : string list) =
    match v with
	s::rest ->
	  if not ((List.nth p n) = s) then
	    output_string outChannel ("<TD ALIGN=RIGHT><FONT COLOR=#FF0000>"^s^"&nbsp;&nbsp;</FONT></TD>")
	  else
	    output_string outChannel ("<TD ALIGN=RIGHT><FONT COLOR=#FFFFFF>"^s^"&nbsp;&nbsp;</FONT></TD>");
	  recPrintHtmlValues (n + 1) p rest
      | [] -> ()
  in
    recPrintHtmlValues n p v;
    output_string outChannel "\n";
;;

(** The following function is used to generate an html header for the
    simulation output
*)

let generateHtmlHeader (sp: Symprog.t) (sm: Symmod.t) (expr: string)
    (nCycles: int) (set: stateset_t) (outChannel: out_channel) : unit =
  let vnames = ref [] in
  let values = ref [] in
  let rec printVars (v : string list) =
    match v with
	s::rest ->
	  output_string outChannel ("<TD ALIGN=RIGHT><U>"^s^"</U>&nbsp;&nbsp</TD>");
	  printVars rest
      | [] -> ()
  in
  let mgr = Symprog.get_mgr sp in
    output_string outChannel ("<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">\n");
    output_string outChannel ("<body style=\"font-family: Courier; color: #FFFFFF;\n");
    output_string outChannel ("background-color: rgb(153, 170, 204);\" alink=\"#000099\" vlink=\"#990099\" link=\"#000099\">\n");
    output_string outChannel ("<TITLE>Simulation Report</TITLE>\n");
    output_string outChannel ("<H2><FONT COLOR=#000000>Simulation Report</FONT></H2>\n");
    output_string outChannel ("<HR><width=100%></HR>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Inputs</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT>Module: "^(Symmod.get_name sm)^"</TD>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT>nCycles: "^(string_of_int nCycles)^"</TD>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT>User initial state: <B><I>\""^expr^"\"</I></B></TD>\n");
    output_string outChannel ("</TABLE>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Computed Start State</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    extractRow (Mlglu.mdd_to_string mgr set) 1 vnames;
    output_string outChannel ("<TR><TD ALIGN=LEFT>Variables:&nbsp;&nbsp;&nbsp;&nbsp;</TD>");
    printVars !vnames;
    output_string outChannel "\n";
    extractRow (Mlglu.mdd_to_string mgr set) 0 values;
    output_string outChannel ("<TR><TD ALIGN=LEFT>Values:&nbsp;&nbsp;&nbsp;&nbsp;</TD>");
    printHtmlValues 0 !values !values outChannel;
    output_string outChannel ("</TR></TABLE>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Simulation</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT><U>Cycle</U>&nbsp;&nbsp;&nbsp;&nbsp;</TD><TD ALIGN=LEFT><U>Action Name</U>&nbsp;&nbsp;&nbsp;&nbsp</TD>");
    printVars !vnames;
    output_string outChannel ("</TR>\n");
;;

let generateHtmlCycle (nCycle: int) (r: Symmod.rule_t) (outChannel: out_channel) : unit =
    output_string outChannel ("<TR><TD ALIGN=LEFT><FONT COLOR=#000000>"^string_of_int(nCycle)^"</FONT>&nbsp;&nbsp;</TD>");
    output_string outChannel ("<TD ALIGN=LEFT><FONT COLOR=#000000>"^(Symmod.get_rule_act r)^"</FONT>&nbsp;&nbsp;</TD>");
;;

let generateHtmlCycleState (nCycle: int) (prev: stateset_t) (set: stateset_t)
    (outChannel: out_channel) : unit =
  let prevValues = ref [] in
  let values = ref [] in
  let mgr = Symprog.get_mgr Symprog.toplevel in
    extractRow (Mlglu.mdd_to_string mgr prev) 0 prevValues;
    extractRow (Mlglu.mdd_to_string mgr set) 0 values;
    printHtmlValues 0 !prevValues !values outChannel;
    output_string outChannel "</TR>\n";
;;

let generateHtmlFooter (outChannel: out_channel) : unit =
  output_string outChannel ("</TABLE>\n");
;;

(** The following function is used to do random simulation. The parameters
    expected are -

    sm         - the symbolic module on which to run the simulation
    expr       - a valid ticc expression representing the initial state
    nCycles    - number of cycles to simulate
    outputFile - the filename to use for the results html file

    Algorithm -
    1. Find the intersection of the user start state with input and output
       invariants
    2. If the intersection is empty exit
    3. if the intersection is non-empty,
       startState = A random minterm from the intersection
    4. loop through statement 8. for nCycles
    5.   stateSet = computeNextState from current state
    6.   nextState = A minterm at random from stateSet
    7.   print nextState
    8.   startState = nextState

*)

let simulate (sm: Symmod.t) (expr: string) (nCycles: int)
    (outputFile: string) : unit =
  let parsedState : stateset_t =
      if expr = "" 
      (* if no expression is given, use the initial condition *)
      then Ops.reachable Symprog.toplevel sm 
      (* otherwise, parser what the user gave *)
      else Symbuild.parse_stateset Symprog.toplevel expr
  in
  let mgr = Symprog.get_mgr Symprog.toplevel in
  let zero = Mlglu.mdd_zero mgr in
  let iinv = Symmod.get_iinv sm in
  let oinv = Symmod.get_oinv sm in
  let vAll = Symmod.get_vars sm in
  let startState : stateset_t =
    Printf.printf "\nBegin simulation %d cycles. One dot per cycle.\n" nCycles;
    flush stdout;
    let iParsedState = Mlglu.mdd_and parsedState iinv 1 1 in
    let oParsedState = Mlglu.mdd_and iParsedState oinv 1 1 in
      if (Mlglu.mdd_equal oParsedState zero) then
	raise User_Start_State_Violates_Invariants;
      Mlglu.mdd_pick_one_minterm mgr oParsedState vAll
  in
  let outChannel : out_channel = open_out outputFile in
  let doSimulate : unit =
    generateHtmlHeader Symprog.toplevel sm expr nCycles startState outChannel;
    let prevState = ref startState in
      for i = 1 to nCycles do
	let nextState : stateset_t =
	  match (computeNextState Symprog.toplevel sm !prevState) with
	      (state, r) ->
		generateHtmlCycle i r outChannel;
		Printf.printf ".";
		flush stdout;
		Mlglu.mdd_pick_one_minterm mgr state vAll
	in
	  generateHtmlCycleState i !prevState nextState outChannel;
	  prevState := nextState;
      done;
      generateHtmlFooter outChannel
  in
    try
      doSimulate;
      Printf.printf "\n";
      Printf.printf "Done simulation. Results in %s.\n" outputFile;
    with User_Start_State_Violates_Invariants ->
      Printf.printf "Start state violates invariants.\n";
;;
