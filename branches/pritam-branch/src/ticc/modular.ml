(* This module defines all functions that can be used to sythesize a concrete
 interface *)

open Str;;
open Symmod;;
open Ticc;;
open Ast;;
open Ops;;
open ExtString;;
open Printf;;

module VarSet = Vset.VS;;


type explicit_t = {
   (* Initial state of the explicit graph *)
   mutable start : Mlglu.mdd;
   (* List of vertices *)
   mutable nodes : Mlglu.mdd list; 
   (* List of edges *)
   mutable edges : (Mlglu.mdd * string * Mlglu.mdd) list;
   (* Lisof Nodes Seen but not Explored *)
   mutable queue : Mlglu.mdd list; 	
}


let add_node (exg : explicit_t) (n : Mlglu.mdd):unit = exg.nodes <- List.append exg.nodes [n]

let add_edge (exg :explicit_t) (c : Mlglu.mdd) (f : string) (n : Mlglu.mdd) : unit = 
 exg.edges <- List.append exg.edges [(c,f,n)]

let insert_queue (exg : explicit_t) (n : Mlglu.mdd):unit = exg.queue <- List.append exg.queue [n]

let remove_queue (exg : explicit_t) : Mlglu.mdd =
  let h = List.hd exg.queue in
  let t = List.tl exg.queue in 
  exg.queue <- t ;
  h
(* build empty explicit graph *)
let build_empty_explicit_graph gl_init : explicit_t = {
  start = gl_init; 
  nodes = []; 
  edges = [];
  queue = [];
}

(** Build explicit graph *)
let build_graph (exg : explicit_t) (av : VarSet.t) (fns : Symmod.t list): unit =
  let sp  =  Symprog.toplevel in
  let mgr = Symprog.get_mgr sp in
  let funcno = List.length fns in
  insert_queue exg exg.start;
  while (List.length exg.queue > 0) do
    let currnode = remove_queue exg in
    for i=0 to funcno -1 do
      let m = List.nth fns i in
      let cv =  ref (VarSet.diff (Symmod.get_vars m) av) in
      let wvarlist = Abstract.get_wvar_list m in
      let unclist = ref (Abstract.create_unchngd_list sp wvarlist av) in
      let (may_trans, must_trans) = Abstract.compute_abstract_trans m true av !cv in
      let nextnode = Abstract.post may_trans currnode av !unclist in
      insert_queue exg nextnode;
    done;	  
  done;;
  

(* a utility function which returns true if a given element has same value as one element in list*)
let rec list_has_node (m : Mlglu.mdd) (mgr:Mlglu.mdd_manager) (l : Mlglu.mdd list): bool =
  match l with
 	[] -> false
       | hd :: tl -> (Mlglu.mdd_equal m hd) || (list_has_node m mgr tl);;
 
(* a utility function which returns the index of the element in the list *)
let rec get_rev_index (m : Mlglu.mdd) (l:Mlglu.mdd list) : int =
  match l with 
        [] -> -1
       | hd :: tl -> 
           if (Mlglu.mdd_equal m hd) then begin
	     let (a,b) = String.split "x y" " " in
	     Printf.printf "%s %s" a b;
             List.length tl
	   end
           else
             (get_rev_index m tl)



(* given a symbolic interface, return the explicit interface graph *)
(* let build_explicit_graph (ig: t) (exg : explicit_t) : unit =
   let sp  =  Symprog.toplevel in
   let mgr = Symprog.get_mgr sp in
   insert_queue exg exg.start;*)
   (* this loop does the traversal and the refinement of nodes *)
   (*while ((List.length  exg.queue) > 0) do 
     let currState = remove_queue exg in*)
     (* check all functions whether currState needs to split *)
     (* if all functions passes the currState then enqueue the nextstates*)
     (* else split the current state and put the new nodes into the queue *)
   (*  let quit_loop = ref false in
     let i = ref 0 in
     
      while (!i < List.length ig.functions && not !quit_loop) do
        if (currStateSplittable currState ig (List.nth ig.functions !i)) then
            begin
            Printf.printf "function  %s"  (List.nth ig.functions !i) ;
            i := !i + 1;
            let x,y = split_state currState ig (List.nth ig.functions !i) in  
            if not ((list_has_node x mgr exg.nodes)||(list_has_node x mgr exg.queue)) then begin
              insert_queue exg x
              end
            else flush stdout;
            if not ((list_has_node y mgr exg.nodes) ||  (list_has_node y mgr exg.queue)) then begin
              insert_queue exg y;
              quit_loop := true 
            end
            else
              flush stdout

            end
        else 
            i := !i + 1
     done;     
     if (not !quit_loop) then begin
        add_node exg currState;
        for i = 0 to (List.length ig.functions) -1 do
           let f = List.nth ig.functions i in
           let sm = Hsetmap.find ig.symmods f in 
           let poststate  =  post_cond sm currState in
           if not ((list_has_node poststate mgr exg.nodes)||(list_has_node poststate mgr exg.queue)) then begin
              insert_queue exg poststate
           end  
       done;
    end;
    done;
    let sink =  Mlglu.mdd_zero mgr in
    add_node exg sink;  *) 
   (* now this loop adds all edges...no refinement done here *)
   (* for i=0 to (List.length exg.nodes) -1 do
      let currState = List.nth exg.nodes i in
      for i = 0 to (List.length ig.functions) -1 do
         let  f = List.nth ig.functions i in 
         let  sm = Hsetmap.find ig.symmods f in 
         let prec = Hsetmap.find ig.preconds f in
	 let precAndcurrState = Mlglu.mdd_and currState prec 1 1 in
         if not (Mlglu.mdd_is_zero precAndcurrState) then
             let nextState = post_cond sm currState in
             add_edge exg currState f nextState
         else
             add_edge exg currState f sink      
      done;
   done;; *)  
    

    
 
(* prints an explicit graph *)
let print_explicit_graph (exg : explicit_t) : unit =
  let sp  =  Symprog.toplevel in
  let mgr = Symprog.get_mgr sp in
  for i=0 to (List.length exg.nodes) -1 do
    Printf.printf "\n state : %d \n MDD value " i; 
    flush stdout;
    Mlglu.mdd_print mgr (List.nth exg.nodes i);
  done;
  for i=0 to (List.length exg.edges) -1 do
    let x,y,z = List.nth exg.edges i in
    Printf.printf "\n edge : %s \n MDD value " y; 
    flush stdout;
    Mlglu.mdd_print mgr x;
    Mlglu.mdd_print mgr z   
  done;;

(* dumps an explicit graph into a dot file *)
let dump_explicit_graph (exg: explicit_t) (ofn : string) : unit =
  let oc = open_out ofn in
  Printf.fprintf oc "digraph G{\n";
  for i=0 to (List.length exg.nodes) -2 do
  Printf.fprintf oc "%d [shape=box,label=\" %d\"]\n" i i;
  done;
  let i = (List.length exg.nodes) -1 in
  Printf.fprintf oc "%d [shape=box,color=crimson,label=\" %d\"]\n" i  i;

  for i=0 to (List.length exg.edges) -1 do
    let x,y,z = List.nth exg.edges i in
    let xind  = (List.length exg.nodes) -1 - (get_rev_index x exg.nodes) in
    let zind  = (List.length exg.nodes) -1 - (get_rev_index z exg.nodes) in
    Printf.fprintf oc "%d -> %d [label=\"%s\"] \n" xind zind y;
  done;
  Printf.fprintf oc "}\n";
  close_out oc;;

(** Function which calls abstraction refinement each function separately *)

let absref (m : Symmod.t ) (av : VarSet.t) (goal: Mlglu.mdd) : VarSet.t =
  let sp  =  Symprog.toplevel in
  let mgr = Symprog.get_mgr sp in
  let result = ref 1 in
  let modinit = get_init m in
  while (!result = 1) do
    let cv =  ref (VarSet.diff (Symmod.get_vars m) av) in
    let wvarlist = Abstract.get_wvar_list m in
    let unclist = ref (Abstract.create_unchngd_list sp wvarlist av) in
    let (may_trans, must_trans) = Abstract.compute_abstract_trans m true av !cv in
    let winm = Abstract.find_winning_states may_trans goal av !unclist in
    let winM = Abstract.find_winning_states must_trans goal av !unclist in
    let mayinit = Mlglu.mdd_and winm modinit 1 1 in
    let mustinit = Mlglu.mdd_and winM modinit 1 1 in
    let diff = Mlglu.mdd_and winm winM 1 0 in
    result := if Mlglu.mdd_is_zero diff then 0 else 1;
  done;  
  av;;

(** The algorithm to obtain an interface *)

let explore (init: Mlglu.mdd) (goal: Mlglu.mdd) (fns: Symmod.t list): unit =
  let sp  =  Symprog.toplevel in
  let mgr = Symprog.get_mgr sp in
  let av = ref (VarSet.union (Mlglu.mdd_get_support mgr goal) (Mlglu.mdd_get_support mgr init)) in
  let funcno = List.length fns in
  for i=0 to funcno -1 do
    let m = List.nth fns i in
    av := absref m !av goal;
  done;
  let exg = build_empty_explicit_graph init in
  build_graph exg !av fns;
  printf "\n blah \n";;







