structure SchBTA : SCHEMEBTA = struct
        datatype bindtime = S | D;

	type btenv = bindtime list * bindtime;
	type division = string -> btenv;     
	
	val concatWith = SchUtil.concatWith;
      
	(* Get a string representation of a binding time environment *)
	fun BtEnvToString (tau,t) =
	    let 
		fun btToString S = "S"
		  | btToString D = "D"
	    in
		"(" ^ (concatWith " " (List.map btToString tau)) ^ ", " ^ (btToString t) ^ ")"
	    end
		;

      
	(* Get a string representation of division. It concatenates the string "f . <btenv>" for each function f defined in prg *)
	fun divisionToString prg divi = 
	    let
		val (fnames,_) = prg
		val strings = List.map (fn f => f ^ " . " ^ (BtEnvToString (divi f))) fnames
	    in
		concatWith "\n" strings
	    end
		;

	(* find the environment corresponding to function f in given list of binding time environments *)
	(* A partial application of this function (divi fnames envs) is used to return the division *)
	(* resulting from binding time analysis *)
	fun divi fnames envs f = SchAST.lookup f fnames envs;

	(* less-than-or-equal operator for binding time dataype *)
        (* t <= t' iff t=S or t=t' *)
	fun bt_lteq (S, _) = true
	  | bt_lteq (D, D) = true
	  | bt_lteq (_, _) = false
	    ;

	(* Returns the lub of binding times t and t' *)
	fun binary_bt_lub (t,t') = if bt_lteq(t,t') then t' else t

	(* Returns the least upper bound of the binding times in list ls *)
        (* Will throw an exception if ls is empty *)
	fun bt_lub ls = List.foldl binary_bt_lub S ls;
		
	(* less-than-or-equal operator for binding time environments *)
	(* ([t1 ... tn],t) <= ([t'1 .. t'n],t') iff ti <= t'i for all i=1,...,n and t <= t'*)    
	fun binary_btenv_lteq (btenv1,btenv2) = 
	    let
		val (tau,t) = btenv1
		val (tau',t') = btenv2
	    in
		List.all (fn t => t) (ListPair.map bt_lteq (t::tau,t'::tau'))
	    end
		;
	    
	(* Make the identity for btenv_lub. Length = length of given list *)
	fun make_bottom ls = List.map (fn _ => S) ls;

	(* Compute the least upper bound of the binding time environments given in list ls *)
        (* Will throw an exception if length ls empty *)
	fun btenv_lub ls = 
	    let
		val (tau,_) = hd ls
		fun helper (btenv1,btenv2) =
		    let
			val (tau1,t1) = btenv1
			val (tau2,t2) = btenv2
			val res = ListPair.map binary_bt_lub (t1::tau1,t2::tau2)
		    in
			(tl res, hd res)
		    end
	    in 
		List.foldl helper (make_bottom tau, S) ls
	    end


	(* Perform binding time analysis with given environment for goal function *)
	fun bta prg tau0 =
	    let 
		val bt_env0 = (tau0,D)
		val (fnames,ls) = prg
		val (formals_list, expr_list) = ListPair.unzip ls

		(* Initialize the division, use input environment for goal function, bottom element for all other functions *)
		val formal_bottoms = List.map make_bottom (tl formals_list)
		val func_bottoms = make_bottom (tl fnames)
		val bt_envs0 = bt_env0 :: (ListPair.zip (formal_bottoms, func_bottoms))


		fun bt_lookup x divi h = 
		    let val (tau,_) = divi h in SchAST.lookup x (SchAST.lookup h fnames formals_list) tau end 

		fun body_lookup fname = (SchAST.lookup fname fnames expr_list);
		  
		  

		(* Compute the binding time of an expression occuring in the body of function h w.r.t. division divi  *)
                (* CHANGEME: Currently returns S *)
 		fun Be divi h e  = 
		  let
		    (*val _ = print ("Calling Be: \n" ^
			  "   divi = " ^ (divisionToString prg divi) ^ "\n" ^
			  "   h = " ^ h ^ "\n" ^
			  "   e = " ^ (SchAST.expToString e) ^ "\n");*)
			  
		    fun be exp = case exp of
			(SchAST.Const _) 	=> S
		      | (SchAST.Variable x) 	=> (bt_lookup x divi h)
		      | (SchAST.If (e1,e2,e3)) => (be_ls [e1,e2,e3])
		      | (SchAST.Call (f, els)) => (bt_lub [(be_ls els), (#2 (divi f))])
		      | (SchAST.Op (_, els))	=> (be_ls els)
		      
		    and be_ls ls = (bt_lub (map be ls));
		  in
		    (be e) 
		  end;

		(* Compute the least upper bound of the binding times in all calls to g in e occuring in the body of function h *)
		fun Bv g divi (h, e) = 
		    let
		        (*val _ = print ("Calling Bv: \n" ^
			  "   g = " ^ g ^ "\n" ^ 
			  "   divi = " ^ (divisionToString prg divi) ^ "\n" ^
			  "   h = " ^ h ^ "\n" ^
			  "   e = " ^ (SchAST.expToString e) ^ "\n");*)

			val (tau,_) = divi g
			val bottom = (make_bottom tau,S)
			
			fun sp_bt f els = if f = g then
			  let
			    val u = map (Be divi h) els;
			  in
			    [(u, (bt_lub [(bt_lub u), (#2 (divi g))]))]
			  end
			  else [];
			    
			fun bv exp = case exp of
			    (SchAST.Const _) => bottom
			  | (SchAST.Variable _) => bottom
			  | (SchAST.If (e1,e2,e3)) => (bv_ls [e1, e2, e3])
			  | (SchAST.Call (f, els)) => (btenv_lub ((bv_ls els)::(sp_bt f els)))
			  | (SchAST.Op (_, els)) => (bv_ls els)
			and bv_ls ls = (btenv_lub (map bv ls));
			
			
		    in
			(bv e)
		    end
			;
		
		(* Compute the least upper bound of the binding times in all calls to g over all expressions in program *)
		(* Computes = |_|i=1 ^n Bv[|ei|] f_i g  |_| t where *)
		(*     t = (bt_env0,D) if g is the goal function, and *)
		(*         ((S,...S),Be[|eg|] div g) otherwise *)
		(*    t passed as return_btenv. See additional notes on BTA in course webpage  *)
		(* Returns a pair (new_btenv,flag) where env is the new binding time environment for g and *)
		(* flag is true if the new environment is less than or equal to (divi g), this indicates a *)
		(* fixed-point for g *)
		fun fp_step divi (g,return_btenv) = 
		  let
		    (* val _ = print ("Calling fp_step:\n" ^
			  "   divi = " ^ (divisionToString prg divi) ^ "\n" ^
			  "   g = " ^ g ^ "\n" ^
			  "   return_btenv = " ^ (BtEnvToString return_btenv) ^ "\n"); *)
			  
		    val base = (map (Bv g divi) (ListPair.zip (fnames, expr_list)));
		    
		    val t = (if g = (hd fnames) 
		      then bt_env0
		      else let
			val (formals,body) = (SchAST.lookup g fnames ls);
		      in
			((map (fn _ => S) formals), (Be divi g body))
		      end)
		      
		    val n_env = btenv_lub (t::base);
		    val stop = binary_btenv_lteq (n_env, (divi g));
		    
		    (* val _ = print ("Result: (" ^ (BtEnvToString n_env) ^ "," ^ Bool.toString(stop) ^ ")\n"); *)
		  in
		    (n_env, stop)
		  end;

		(* Get the next point in the iterative solution of analysis equations *)
		(* Returns input environments if next point is less than or equal to input *)
		fun get_fixpoint bt_envs = 
		    let 
			val divi = divi fnames bt_envs
			fun get_ret_val_btenv (g,e) =
			    let 
				val (tau,_) = divi g
				val formals_btenv = make_bottom tau
				val ret_val_bt = Be divi g e
			    in
				 (formals_btenv, ret_val_bt)
			    end
			val return_btenvs = ListPair.map get_ret_val_btenv ((tl fnames),(tl expr_list))
			val return_btenvs = bt_env0::return_btenvs
				      
			(* Zip function names and formals list to setup call to fp_step *)
			val fdefs = ListPair.zip (fnames, formals_list)
			val new_btenvs_flags_pairs = ListPair.map (fp_step divi) (fnames,return_btenvs)

			(* Unzip environments and fixpoint flags *)
			val (new_btenvs, flags) = ListPair.unzip new_btenvs_flags_pairs
			val flag = List.all (fn a => a) flags
		    in
			if flag then new_btenvs else (get_fixpoint new_btenvs)
		    end
			;
		
		(* Solve the analysis equations until a fixpoint is reached *)
		val bt_envs = get_fixpoint bt_envs0
		
	    in
		divi fnames bt_envs
	    end
		;

end
