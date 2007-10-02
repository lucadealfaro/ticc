(** approx.ml *)

(** ********************* Pairwise Composition of Modules*************** *)

(** The function takes two lists of modules and clones all of these symbolic 
    modules. 
    It returns an union of those cloned modules with changed input invariants 
    after composing each module of the 
    first list with each module from the second list of modules.
    The boolean argument "optflag" is true(resp. false) for optimistic 
    i.e. \exists (resp. pessimistic i.e. \forall) input invariant generation.  
*)
  
let pairwise_composition (sp:Symprog.t) optflag win_algo ?(result_name="") (ml1:Symmod.t list) (ml2:Symmod.t list) : Symmod.t list =
  
   
  let newml1 = symbolic_clone ml1 in
  (*List.iter (Symmod.print_symmod) newml1;*)
  let newml2 = symbolic_clone ml2 in
  (*List.iter (Symprog.print_symmod) newml2;*)

  let mgr = Symprog.get_mgr sp in
   (* modTable is a hash table from  modules to input invariants *) 
   let modTable = Hashtbl.create (List.length ml1 + List.length ml2) in
   let _ = (List.map 
      (fun m1 -> List.map 
         (fun m2 ->
          (* resultMod = m1 || m2 *) 
          let resultMod = composition sp win_algo m1 m2 in
          (* resModIinv = input invariant of m1 || m2  *)
          let resModIinv = Symmod.get_iinv resultMod in
          (* vAll is the set of variables in m1 || m2  *)
          let vAll = Symmod.get_vars resultMod in
          (* vAllMinusM1 = vALL - vM1(variables of M1) *)   
          let vAllMinusM1 = VarSet.diff vAll (Symmod.get_vars m1) in
          (* invm1 = \forall vAllMinusM1 resModIinv*)   
          let invm1 = 
            match optflag with
	      true  ->Mlglu.mdd_smooth mgr resModIinv vAllMinusM1  
	    | false ->Mlglu.mdd_consensus mgr resModIinv vAllMinusM1 
          in
          (* if m1 in modTable, modTable[m1] = invm1 /\ modTable[m1]*)
          (* otherwise,         modTable[m1] = invm1 /\ inv of M1   *) 
          (if Hashtbl.mem modTable m1 
           then let value = (Hashtbl.find modTable m1) in
            Hashtbl.replace modTable m1 (Mlglu.mdd_and value invm1 1 1)  
           else Hashtbl.add modTable m1 
                (Mlglu.mdd_and (Symmod.get_iinv m1) invm1 1 1));
          (* vAllMinusM2 = vALL - vM2(variables of M2)         *)   
          let vAllMinusM2 = VarSet.diff vAll (Symmod.get_vars m2) in
          (* invm2 = \forall vAllMinusM2 resModIinv *)
          let invm2 = 
	    match optflag with
	      true  ->Mlglu.mdd_smooth mgr resModIinv vAllMinusM2  
	    | false ->Mlglu.mdd_consensus mgr resModIinv vAllMinusM2 
	  in
          (* if m2 in modTable, modTable[m2] = invm2 /\   modTable[m2] *)
          (* otherwise,         modTable[m2] = invm2 /\   inv of M2    *) 
          (if Hashtbl.mem modTable m2 
           then let value = (Hashtbl.find modTable m2) in
                Hashtbl.replace modTable m2 (Mlglu.mdd_and value invm2 1 1)  
           else Hashtbl.add modTable m2 
	      (Mlglu.mdd_and (Symmod.get_iinv m2) invm2 1 1));
         ()
        ) 
   newml2) 
   newml1) 
  in 
  (* Foreach module-iinv pair (m,i) in modTable, addList constructs an 
      element of the list as a module with i as its input invariant 
   *)
  let addList m i l = List.append l [((Symmod.set_iinv m i);m)] in 
  (Hashtbl.fold addList modTable [])
 
 

(** composition_test2

    This function is based on the crazy idea that the input invariant
    for a set of composed modules is just the conjunction of all the
    invariants derived by composing them pairwise.

    It only returns the final invariant calculated.
 *) 
let composition_test2 (sp:Symprog.t) win_algo (ml:Symmod.t list) =
  let mgr = Symprog.get_mgr sp in
  let result_iinv = ref (Mlglu.mdd_one mgr) in
  let result_oinv = ref (Mlglu.mdd_one mgr) in
  let rec compose_one_with_list (m:Symmod.t) (lst:Symmod.t list) =
    match lst with
      [] ->  ()
    | hd::tl ->
        let resultMod = composition sp win_algo m hd in
        let resModIinv = Symmod.get_iinv resultMod in
        let resModOinv = Symmod.get_oinv resultMod in
	result_iinv := Mlglu.mdd_and !result_iinv resModIinv 1 1;
	result_oinv := Mlglu.mdd_and !result_oinv resModOinv 1 1;
	compose_one_with_list m tl;
  in
  let rec composition_list (ml:Symmod.t list) =
    match ml with
      [] -> ()
    | hd::tl ->
	compose_one_with_list hd tl;
	composition_list tl;
  in
  composition_list ml;
  Mlglu.mdd_and !result_iinv !result_oinv 1 1;
