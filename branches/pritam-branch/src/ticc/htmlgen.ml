(** This module contains the implementation of HTML generation functions and all
    of their support functions
*)

open Str;;
open Symmod;;

module VarSet = Vset.VS;;

(** This function is used to parse a state string and return either the
    variables or their values in a list *)

let extractRow (state : string) (wantVars : int) (result : string list ref) 
    (allVarNames: string list option) : unit =
  let splitState = Str.split (Str.regexp " ") state in
  let rec extractNames (splitList : string list) : unit =
    match splitList with
	s::rest -> begin
	  let nameValue (s : string) = Str.split (Str.regexp "=") s in
	    begin
	      match (nameValue s) with
		  name::value ->
		    result := !result@[name]
		| _ -> ();
	    end;
	    extractNames rest;
	end;
      | _ -> ()	      
  in
  let index = ref 0 in
  let rec extractValues (splitList : string list) : unit =
    match splitList with
	s::rest -> begin
	  let nameValue (s : string) = Str.split (Str.regexp "=") s in
	    match (nameValue s) with
		name::value -> begin
		  match allVarNames with
		      Some(names) ->
			if (name = (List.nth names !index)) then begin
			  result := !result@value;
			  index := !index + 1;
			  extractValues rest;
			end else begin
			  result := !result@["*"];
			  index := !index + 1;
			  extractValues splitList;
			end;
		    | None ->
			result := !result@value;
			extractValues rest;
		end;
	      | _ -> ();
	end
      | [] -> ();
  in
    if (wantVars = 1) then
      extractNames splitState
    else
      extractValues splitState
;;

(** The following function is used to print values in a table row *)

let printHtmlValues (n : int) (p : string list) (v : string list)
    (outChannel: out_channel) : unit =
  let rec recPrintHtmlValues (n : int) (p : string list) (v : string list) =
    match v with
	s::rest -> begin
	  if ((List.length p) > n) then begin
	    if not ((List.nth p n) = s) then
	      output_string outChannel 
		("<TD ALIGN=RIGHT><FONT COLOR=#FF0000>"^s^"&nbsp;&nbsp;</FONT></TD>")
	    else
	      output_string outChannel 
		("<TD ALIGN=RIGHT><FONT COLOR=#FFFFFF>"^s^"&nbsp;&nbsp;</FONT></TD>");
	  end else
	    output_string outChannel 
	      ("<TD ALIGN=RIGHT><FONT COLOR=#FF0000>"^"*"^"&nbsp;&nbsp;</FONT></TD>");
	  recPrintHtmlValues (n + 1) p rest;
	end;
      | [] -> ()
  in
    recPrintHtmlValues n p v;
    output_string outChannel "\n";
;;

(** The following function is used to generate a html header *)

let generateHtmlHeader (sp: Symprog.t) (sm: Symmod.t) (set: Mlglu.mdd) 
    (title: string) (outChannel: out_channel) : unit =
  let vAll = Symmod.get_vars sm in
  let mgr = Symprog.get_mgr sp in
  let completeInit = Mlglu.mdd_pick_one_minterm mgr set vAll in
  let vnames = ref [] in
  let values = ref [] in
  let rec printVars (v : string list) =
    match v with
	s::rest ->
	  output_string outChannel ("<TD ALIGN=RIGHT><U>"^s^"</U>&nbsp;&nbsp</TD>");
	  printVars rest
      | [] -> ()
  in
    output_string outChannel 
      ("<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">\n");
    output_string outChannel ("<body style=\"font-family: Courier; color: #FFFFFF;\n");
    output_string outChannel 
      ("background-color: rgb(133, 150, 180);\" alink=\"#000099\" vlink=\"#990099\" link=\"#000099\">\n");
    output_string outChannel ("<TITLE>"^title^"</TITLE>\n");
    output_string outChannel ("<H2><FONT COLOR=#000000>"^title^"</FONT></H2>\n");
    output_string outChannel ("<HR><width=100%></HR>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Module</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT>"^(Symmod.get_name sm)^"</TD>\n");
    output_string outChannel ("</TABLE>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Start State</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    extractRow (Mlglu.mdd_to_string mgr completeInit) 1 vnames None;
    output_string outChannel ("<TR><TD ALIGN=LEFT>Variables:&nbsp;&nbsp;&nbsp;&nbsp;</TD>");
    printVars !vnames;
    output_string outChannel "\n";
    extractRow (Mlglu.mdd_to_string mgr completeInit) 0 values None;
    output_string outChannel ("<TR><TD ALIGN=LEFT>Values:&nbsp;&nbsp;&nbsp;&nbsp;</TD>");
    printHtmlValues 0 !values !values outChannel;
    output_string outChannel ("</TR></TABLE>\n");

    output_string outChannel ("<H3><U><FONT COLOR=#000000>Trace</FONT></U></H3>\n");
    output_string outChannel ("<TABLE BORDER=0>\n");
    output_string outChannel ("<TR><TD ALIGN=LEFT><U>Action Name</U>&nbsp;&nbsp;&nbsp;&nbsp</TD>");
    printVars !vnames;
    output_string outChannel ("</TR>\n");
;;

let generateHtmlCycle (sm: Symmod.t) (r: Symmod.rule_t) (prev: Mlglu.mdd) (set: Mlglu.mdd) 
    (outChannel: out_channel) (sr: Symmod.rule_t option) : unit =
  let vAll = Symmod.get_vars sm in
  let mgr = Symprog.get_mgr Symprog.toplevel in
  let init = Symmod.get_init sm in
  let completeInit = Mlglu.mdd_pick_one_minterm mgr init vAll in
  let allVarNames = ref [] in
  begin
    match sr with
	Some(rule) ->
	  output_string outChannel 
	    ("<TD ALIGN=LEFT><FONT COLOR=#00FFAA><U>"^(Symmod.get_rule_act r)^
	       "</U></FONT>&nbsp;&nbsp;</TD>");
      | None ->
	  output_string outChannel 
	    ("<TD ALIGN=LEFT><FONT COLOR=#000000>"^
	       (Symmod.get_rule_act r)^"</FONT>&nbsp;&nbsp;</TD>");
  end;
  let prevValues = ref [] in
  let values = ref [] in
    extractRow (Mlglu.mdd_to_string mgr completeInit) 1 allVarNames None;
    extractRow (Mlglu.mdd_to_string mgr prev) 0 prevValues (Some(!allVarNames));
    extractRow (Mlglu.mdd_to_string mgr set) 0 values (Some(!allVarNames));
    printHtmlValues 0 !prevValues !values outChannel;
    output_string outChannel "</TR>\n";
;;

let generateHtmlFooter (outChannel: out_channel) : unit =
  output_string outChannel ("</TABLE>\n");
;;
