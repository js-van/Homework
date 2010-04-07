structure LamAST :  LAMAST = struct

    datatype exp = 
	     Var of string
	   | Quote of exp
	   | If of exp * exp * exp
	   | Lambda of string list * exp
	   | Ap of exp * exp list
	   | Let of string list * exp list * exp
	   | Letrec of string list * exp list * exp
	     ;

    type tvname = int list;
    datatype lamtype = 
	     TVar of tvname
	   | TCons of string * lamtype list
    and gentype = Generic of tvname list * lamtype;
    type subst = tvname -> lamtype;

    exception LamTypeError of string * subst * string;

    (*****************************
     *  some type utilities      *
     *****************************)

    (* type expression creation utilities *)
    val int_t = TCons ("int",nil);
    val bool_t = TCons ("bool",nil);
    val sym_t = TCons ("sym",nil);
    infix 5 arrow_t;
    fun op arrow_t (ts,t) = TCons ("arrow",(t::ts));
    fun list_t t = TCons ("list",[t]);

    (* find the type variables in given type expression *)
    fun tvars_in t =
	let
	    fun tvars_in' ((TVar tvn),ls) = tvn::ls
	      | tvars_in' (TCons (name,ts),ls) = List.foldl tvars_in' ls ts
	in
	    tvars_in' (t,nil)
	end

    	    ;

    (************************** 
     * some list utilities    *
     **************************)
    (* is x in list ls? *)
    fun in_list x ls = List.exists (fn y => (x=y)) ls;

    (* remove from xs any members of ys *)
    fun bar xs ys = List.filter (fn x => not (in_list x ys)) xs;

    (* remove duplicates from list *)
    fun nodups tvns = List.foldr (fn (tvn,ls) => if in_list tvn ls then ls else (tvn::ls)) nil tvns;

    (*******************************************
     * utilities for type variable names       *
     * we use int lists to represent           *
     * var name supplies as specified in       *
     * Ch. 9 of Peyton Jones                   *			 
     *******************************************)
    (* the first variable name ever used *)
    val seed_ns = [0];

    (* get the next name from the name supply *)
    fun next_name ns = ns;

    (* make sure we don't repeat a name *)
    fun deplete ns = (hd ns)+2::(tl ns);

    (* make two name supplies that don't overlap *)
    fun split ns = (0::ns,1::ns);

    (* get n names from name supply *)
    fun name_sequence ns 0 = nil
      | name_sequence ns n = (next_name ns) :: (name_sequence (deplete ns) (n - 1));

    (* get pretty var name e.g. 'a 'b etc. *)
    fun getVarName tvn tvns = 
	let
	    fun index_list nil _ = []
	      | index_list (x::xs) n = n::(index_list xs (n+1))
	    val res = List.find (fn (_,tvn')=>tvn=tvn') (ListPair.zip (index_list tvns 1,tvns))
	in
	    case res of
		NONE => (SchUtil.get_name ((length tvns)+1),tvns@[tvn])
	      | SOME (idx,_) => (SchUtil.get_name idx,tvns)
 	end
	    ;

    (* get var name as list of integers *)
    fun getIntVarName tvn _ = (getIntVarName' tvn,nil)
    and getIntVarName' tvn = "["^ (SchUtil.concatWith ":" (List.map Int.toString tvn)) ^ "]"

    (*******************************
     * printing utilities          *
     *******************************)

   (* get a string representation of a lambda-calc expression *)			     
    fun expToString (Var x) = x
      | expToString (Quote e) = "(quote " ^ (expToString e) ^ ")"
      | expToString (If (e1,e2,e3)) = 
	"(if " ^ (SchUtil.concatWith " " (List.map expToString [e1,e2,e3])) ^ ")"
      | expToString (Lambda (formals,body)) =
	"(\\" ^ (SchUtil.concatWith " * " formals) ^ "." ^ (expToString body) ^ ")"
      | expToString (Ap (f,actuals)) =
	"(" ^ (expToString f) ^ " " ^ (SchUtil.concatWith " " (List.map expToString actuals)) ^ ")"
      | expToString (Let (xs,es,e)) =
	"let\n" ^
	(SchUtil.concatWith "\n" (ListPair.map (fn (x,e) => " " ^ x ^ " = " ^ (expToString e)) (xs,es))) ^
	"\nin " ^ (expToString e) ^ ")"
      | expToString (Letrec (xs,es,e)) =
	"(letrec\n" ^ 
	(SchUtil.concatWith "\n" (ListPair.map (fn (x,e) => "  " ^ x ^ " = " ^ (expToString e)) (xs,es))) ^
	"\nin " ^ (expToString e) ^ ")"
	;

    (* get a string representation of a type expression *)
    fun typToString t = typToString' getVarName t
    and typToString' var_printer t = #1(typToString'' var_printer (t,nil))
    and typToString'' var_printer (TVar tvn,tvns) = var_printer tvn tvns
      | typToString'' _ (TCons ("int",_),tvns) = ("int",tvns)
      | typToString'' _ (TCons ("bool",_),tvns) = ("bool",tvns)
      | typToString'' _ (TCons ("sym",_),tvns) = ("sym",tvns)
      | typToString'' var_printer (TCons ("arrow",ts),tvns) =
	let
	    val (tstrs,tvns') = typListToString var_printer tvns (tl ts)
	    val operands_str = SchUtil.concatWith " * " tstrs
	    val (result_str,tvns'') = typToString'' var_printer ((hd ts),tvns')
	in
	    (operands_str ^ " -> " ^ result_str,tvns'')
	end
      | typToString'' var_printer (TCons ("list",[t]),tvns) = 
	let val (tstr,tvns') = typToString'' var_printer (t,tvns) in (tstr ^ " list",tvns') end
      | typToString'' _ _ = raise SchUtil.SchError "can't print unknown type"
    (* get string representations for a list of type expressions *)				  
    and typListToString var_printer tvns ts =
	let
	    fun do_one (t,(tstrs,tvns)) = 
		let val (tstr,tvns') = typToString'' var_printer (t,tvns) in (tstr::tstrs,tvns') end
	in
	    List.foldr do_one (nil,tvns) ts
	end
	    ;

    (* get a string representation of a type environment *)
    fun tEnvToString var_printer tenv = tEnvToString' var_printer nil tenv
    and tEnvToString' _ _  nil = ""
      | tEnvToString' var_printer tvns ((x,gent)::rest) = 
	let
	    val (gents,tvns') = genTypToString var_printer (gent,tvns)
	    val rests = tEnvToString' var_printer tvns' rest
	in
	    x ^ ":" ^ gents ^ "\n" ^ rests
	end
    (* get a string representation of a generic type expression *)
    and genTypToString var_printer ((Generic (nil, t)),tvns) = typToString'' var_printer (t,tvns)
      | genTypToString var_printer ((Generic (gvs, t)),tvns) =
	let
	    val (ts,tvns') = typToString'' var_printer (t,tvns)
	    fun do_one (gvar,(gvars,tvns)) =
		let val (gvarstr,tvns') = var_printer gvar tvns
		in (gvarstr::gvars,tvns') end
	    val (gvss,tvns'') = List.foldr do_one (nil,tvns') gvs
	in
	    ("FORALL " ^ (SchUtil.concatWith " " gvss) ^ ": " ^ ts,tvns'')
	end
	;

    (*************************************
     * substitutions: we use functional  *
     * representation as used in Ch.9 of *
     * Peyton Jones                      *
     *************************************)
    (* the identity substitution *)
    fun id_subst tvn = TVar tvn; 

    (* substitution that only moves one variable *)
    fun delta tvn t tvn' = if tvn = tvn' then t else TVar tvn'; (* only affects one variable *)

    (* return the type of expression w.r.t to substitution aka recursive realization*)
    fun sub_type phi t = case t of
	    TVar(tvn)   => phi tvn
	  | TCons(c,ts) => TCons(c, (map (sub_type phi) ts));
    
    (* compose two substitutions *)
    fun scomp sub2 sub1 tvn = sub_type sub2 (sub1 tvn);

    (* apply a substitution to unkown type variables in generic type expression *)
    fun sub_generic phi (Generic (gvs,t)) =
	let  
	    fun exclude phi gvs tvn = if (in_list tvn gvs) then TVar tvn else phi tvn
	in
	    Generic (gvs, sub_type (exclude phi gvs) t)
	end;

    (* apply substitution to all generic type expresions in an environment *)
    fun sub_te phi gamma = List.map (fn (x,gt) => (x,sub_generic phi gt)) gamma 

   (* print substitution for given list of type variable names *)	    
   fun print_subst phi tvns = 
       let
	   fun print_var tvn = (getIntVarName' tvn) ^ " : " ^ (typToString' getIntVarName (sub_type phi (TVar tvn)))
       in
	   SchUtil.concatWith "\n" (List.map print_var tvns)
       end
	   ;

    (* extend substitution to map type variable to type expression *)
    fun extend phi tvn t = 
      (if t = TVar(tvn) then phi
      else if (List.exists (fn tt => tt = tvn) (tvars_in t)) then
	raise LamTypeError ("Occurs check fails: " ^ (getIntVarName' tvn) ^ " in " ^ (typToString' getIntVarName t),phi,"")
      else scomp (delta tvn t) phi)
    
    (**********************************
     *  unification algorithm         *
     **********************************)
   (* unify type expressions t1 and t2 by extending substitution phi *)
    fun unify phi (t1,t2) = 
      (case t1 of
	TVar(tvn) => let
	  val phitvn = phi tvn;
	  val phit = sub_type phi t2;
	in if phitvn = t1
	then 	(extend phi tvn phit)
	else	unify phi (phitvn, phit)
	end
      | TCons(tn1,ts1) => (case t2 of
	  TVar _ => unify phi (t2,t1)
	| TCons(tn2, ts2) => 
	    if tn1 = tn2
	    then unifyl phi (ListPair.zip (ts1, ts2))
	    else 
		raise LamTypeError (("Fail to unify " ^ (typToString' getIntVarName t1) ^ " and " ^ (typToString' getIntVarName t2)),phi,"")))

   (* unify a list of type expressions *)				
    and unifyl phi eqns = List.foldr (fn ((t1,t2),phi) => unify phi (t1,t2)) phi eqns
	;

    (***************************************
     *  type environments                  *
     *  we are using association lists     *
     ***************************************)

    (* lookup a value in an association list *)
    fun al_lookup _ nil = NONE
      | al_lookup x ((x',t)::rest) =
	if x = x' then SOME t else al_lookup x rest
	;

    (*turn an association list into a substitution *)
    fun al_to_subst al tvn = 
	case al_lookup tvn al of
	    NONE => TVar tvn
	  | SOME tvn' => TVar tvn'
	;



    

    (* generic type variable utilities *)

    (* create a new generic type expression and bind it to x *)
    fun new_bvar (x,tvn) = (x,Generic (nil, TVar tvn));

    (* create new generic type expressions and bind them to xs *)
    fun new_bvars xs ns = ListPair.map new_bvar (xs,ns);

    (* get the type expression for x *)
    fun old_bvar (x,Generic (_,t)) = t;

    (* find the unknown (unbound) variables in a generic type expression *)
    fun unknowns_generic (Generic (gvs,t)) = bar (tvars_in t) gvs;

    (* find all unknown (unbound) variables in type environment *)
    fun unknowns_te gamma = List.concat (List.map unknowns_generic (List.map (fn (x,t) => t) gamma));

    val meaningless_exception = LamTypeError ("",id_subst,"");

    (*Adds declarations to gamma*)
    fun add_decls gamma ns xs ts =
	let
	  val unknowns = unknowns_te gamma;
	  val schemes = map (genbar unknowns ns) ts;
	in
	  List.concat [(ListPair.zip (xs, schemes)), gamma]
	end
    and genbar unknowns ns t =
	let
	  val scvs = bar (nodups (tvars_in t)) unknowns;
	  val al = ListPair.zip (scvs, (name_sequence ns (length scvs)));
	  val tt = sub_type (al_to_subst al) t;
	in
	  Generic ((map #2 al), tt)
	end;	  

    (* Algorithm W *)
    (* type check a list of expressions, using type checker tc_main *)
    (* Two-step function: forward step infers type of each expression in order *)
    (* Resulting substitution for each expression is applied to type env before continuing with tail *)
    (* backward step applies the substition resulting from the tail to result of forward step *)
    (* composes that substitution with the substitution from forward step *)
    fun tcl gamma ns es =
	let 
	    (* forward step function *)
	    fun f (e,(gamma,ns,pairs)) = 
		let 
		    val (ns0,ns1) = split ns
		    val (t,phi) = tc_main gamma ns1 e (* type check e using gamma as type environment *)
		in
		    (* apply result subst to env, append result type and subst to return list *)
		    (sub_te phi gamma, ns0, pairs @ [(t,phi)]) 
		end
	    val (_,_,pairs) = List.foldl f (gamma,ns,nil) es (* does forward step *)
            (* backward step function *)
            (* apply substitution psi from expressions in tail to result of forward step *)
	    (* compose psi with subst phi from forward step *)
	    fun g ((t,phi),(ts,psi)) = ((sub_type psi t) :: ts, scomp psi phi)
	in
	    List.foldr g (nil,id_subst) pairs (* does backward step *)
	end

    (* type check expressions *)
    and tc_var gamma ns v =
      let
	(* 1 *)
	val ev = al_lookup v gamma;
      in case ev of
	  (*2*)
	  SOME (Generic(gvs,t)) =>  let
	    val (ns0, ns1) = split ns;
	    (* a *)
	    val ns2 = name_sequence ns0 (length gvs);
	    (* b *)
	    val ass_list = ListPair.zip (gvs, ns2);
	    (* c *)
	    val phi = al_to_subst ass_list;
	    (* d *)
	    val nt = sub_type phi t;
	  in
	    (* e *)
	    (nt, id_subst)
	  end
	  (*3*)
	| NONE =>  (TVar (next_name ns), id_subst)
      end
    and tc_if gamma ns e1 e2 e3 = 
      let
	(* Create name source *)
	val (ns0, ns1) = split ns;
	(* 1 *)
	val (t1, phi1) = tc_main gamma ns0 e1;
	(* 2 *)
	val phi' = unify phi1 (bool_t, t1);
	(* 3 *)
	val ngamma = sub_te phi' gamma;
	(* 4 *)
	val (t1::t2::[], psi) = tcl ngamma ns1 [e2, e3];
	(* 5 *)
	val phi'' = unify (scomp psi phi') (t1, t2);
	(* 6 *)
	val t = sub_type phi'' t1;
      in
	(t, phi'')
      end
    and tc_ap gamma ns e1 es = 
      let
	val (ns0, ns1) = split ns;
	(* 1 *)
	val (t1::ts, phi0) = tcl gamma ns0 (e1::es)
	(* 2 *)
	val tvn = ns1;
	val body_t = (ts arrow_t (TVar ns1));
	val phi1 = unify phi0 (t1, body_t);
	(* 3 *)
	val rt = phi1 tvn;
      in
	(rt, phi1)
      end
    and tc_lambda gamma ns xs e =
      let
	val (ns0, ns1) = split ns;
	(* 1 *)
	val nls = name_sequence ns0 (length xs);
	(* 2 *)
	val bvars = new_bvars xs nls;
	(* 3 *)
	val ngamma = List.concat [bvars, gamma];
	(* 4 *)
	val (t, phi) = tc_main ngamma ns1 e;
	(* 5 *)
	val ts = map phi nls;
	val rt = (ts arrow_t t);
      in
	(rt, phi)
      end
    and tc_let gamma ns xs es e =
      let
	val (ns0, nst) = split ns;
	val (ns1, ns2) = split nst;
	(* 1 *)
	val (ts, phi) = tcl gamma ns0 es;
	(* 2a *)
	val gamma' = (sub_te phi gamma);
	(* 2b *)
	val gamma'' = add_decls gamma' ns1 xs ts;
	(* 3 *)
	val (t, phi') = tc_main gamma'' ns2 e;
      in
	(t, (scomp phi' phi))
      end
    and tc_letrec gamma ns xs es e =
      let
	(*Create name vars*)
	val (ns0, ns')  = split ns;
	val (ns1, ns'') = split ns';
	val (ns2, ns3)  = split ns'';
	(* 1 *)
	val nbvs = new_bvars xs (name_sequence ns0 (length xs));
	(* 2 *)
	val gamma' = List.concat [nbvs, gamma];
	(* 3 *)
	val (ts, phi) = tcl gamma' ns1 es;
	(* 4 *)
	val nbvs' = sub_te phi nbvs;
	val ts' = map old_bvar nbvs';
	val phi' = unifyl phi (ListPair.zip (ts, ts'));
	val gamma' = sub_te phi' (sub_te phi gamma);
	(* 5 *)
	val nbvs'' = sub_te phi' nbvs';
	val ts'' = map old_bvar nbvs'';
	val gamma'' = add_decls gamma' ns2 (map #1 nbvs'') ts'';
	
	(* 6 *)
	val (rt, phi'') = tc_main gamma'' ns3 e;
      in
	(rt, (scomp phi'' phi'))
      end
    and tc_main gamma ns e = 
	let
	
	    fun f (Var v) = tc_var gamma ns v
	      | f (Quote e) = tc_main gamma ns e
	      | f (If (e1,e2,e3)) = tc_if gamma ns e1 e2 e3
	      | f (Ap (e1,es)) = tc_ap gamma ns e1 es
	      | f (Lambda (xs,e)) = tc_lambda gamma ns xs e
	      | f (Let (xs,es,e)) = tc_let gamma ns xs es e
	      | f (Letrec (xs,es,e)) = tc_letrec gamma ns xs es e
	   
	in
	    f e
	end
	    handle LamTypeError (error,subst,context) =>
		   if context = "" 
		   then 
		       let
			   fun get_ids (Var v) = [v]
			     | get_ids (Quote e) = get_ids e
			     | get_ids (If (e1,e2,e3)) = List.concat (List.map get_ids [e1,e2,e3])
			     | get_ids (Lambda (_,e)) = get_ids e
			     | get_ids (Ap (e1,e2)) = List.concat (List.map get_ids (e1::e2))
			     | get_ids (Let (_,es,e)) = List.concat (List.map get_ids (e::es))
			     | get_ids (Letrec (_,es,e)) = List.concat (List.map get_ids (e::es))
			   fun prune_env gamma e = 
			       let
				   val ids = nodups (get_ids e)
			       in
				   List.filter (fn (id,_) => in_list id ids) gamma
			       end
			   val tenv_str = tEnvToString getVarName (sub_te subst (prune_env gamma e))
			   val e_str = expToString e
		       in
			   raise LamTypeError (error,subst,("expression: " ^ e_str ^ "\nenvironment:\n" ^ tenv_str))
		       end
		   else raise LamTypeError (error,subst,context)
	;

    (* initial type enviroment contains types for primitives and dummy variables for constants *)
    fun make_init_env ns = [
      ("_int", Generic(nil, int_t)),
      ("_bool", Generic(nil, bool_t)),
      ("_symbol", Generic(nil, sym_t)),
      ("_nil", Generic([[~1]], list_t(TVar([~1])))),
      ("car", Generic ([[~1]],
	[list_t(TVar([~1]))] arrow_t TVar([~1]))),
      ("cdr", Generic ([[~1]],
	[list_t(TVar([~1]))] arrow_t list_t(TVar([~1])))),
      ("cons", Generic ([[~1]],
	[TVar([~1]), list_t(TVar([~1]))] arrow_t list_t(TVar([~1])))),
      ("null?", Generic ([[~1]],
	[list_t(TVar([~1]))] arrow_t bool_t)),
      ("+", Generic (nil, [int_t, int_t] arrow_t int_t)),
      ("-", Generic (nil, [int_t, int_t] arrow_t int_t)),
      ("*", Generic (nil, [int_t, int_t] arrow_t int_t)),
      ("div", Generic (nil, [int_t, int_t] arrow_t int_t)),
      ("mod", Generic (nil, [int_t, int_t] arrow_t int_t)),
      ("=", Generic (nil, [int_t, int_t] arrow_t bool_t))];

    fun tc exp = 
	let 
	    val (ns0,ns1) = split seed_ns
	in
	    SOME (tc_main (make_init_env ns1) ns0 exp)
	end
	    handle LamTypeError (error,_,context) => (print (error ^ "\n" ^ context ^ "\n"); NONE)
	;
end
