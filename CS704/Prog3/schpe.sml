structure SchPE : SCHEMEPE = struct
	datatype expra =
	    Const of SchAST.constant
          | Variable of string
	  | IfS of expra * expra * expra
	  | IfD of expra * expra * expra
	  | CallS of string * expra list * expra list
	  | CallD of string * expra list * expra list
	  | OpS of string * expra list
	  | OpD of string * expra list
	  | Lift of expra
		 ;

	type programa = (string list) * ((string list * string list * expra) list);
	
	type pend_func = (string * SchAST.constant list);

	val concatWith = SchUtil.concatWith;

	fun exprAToString (Const c) = SchAST.constToString(c)
	  | exprAToString (Variable v) = v
	  | exprAToString (IfS (e1,e2,e3)) =
	    let
		val sub_exps = List.map exprAToString [e1,e2,e3]
	    in
		"(ifs " ^ (concatWith " " sub_exps) ^ ")"
	    end
	  | exprAToString (IfD (e1,e2,e3)) =
	    let
		val sub_exps = List.map exprAToString [e1,e2,e3]
	    in
		"(ifd " ^ (concatWith " " sub_exps) ^ ")"
	    end
	  | exprAToString (CallS (f,vs,vd)) =
	    let
		val static_string = "(" ^ (concatWith " " (List.map exprAToString vs)) ^ ")"
		val dynamic_string = "(" ^ (concatWith " " (List.map exprAToString vd)) ^ ")"
	    in
		"(calls " ^ f ^ " " ^ static_string ^ " " ^ dynamic_string ^ ")"
	    end
	  | exprAToString (CallD (f,vs,vd)) =
	    let
		val static_string = "(" ^ (concatWith " " (List.map exprAToString vs)) ^ ")"
		val dynamic_string = "(" ^ (concatWith " " (List.map exprAToString vd)) ^ ")"
	    in
		"(calld " ^ f ^ " " ^ static_string ^ " " ^ dynamic_string ^ ")"
	    end
	  | exprAToString (OpS (prim, args)) =
	    let
		val arg_string = concatWith " " (List.map exprAToString args)
	    in
		"(" ^ prim ^ "s " ^ arg_string ^ ")"
	    end
	  | exprAToString (OpD (prim, args)) =
	    let
		val arg_string = concatWith " " (List.map exprAToString args)
	    in
		"(" ^ prim ^ "d " ^ arg_string ^ ")"
	    end
	  | exprAToString (Lift expra) = "(lift " ^ (exprAToString expra) ^ ")"
	    ;
	
	(* Make a string containing the given annotated Scheme0 program *)
	fun progAToString (fnames,formals_envs) = 
	    let
		fun funcAToString (f, (static_formals, dynamic_formals, expa)) =
		    let
			val static_string = "(" ^ (concatWith " " static_formals) ^ ")"
			val dynamic_string = "(" ^ (concatWith " " dynamic_formals) ^ ")"
			val body = (exprAToString expa)
		    in
			"(define (" ^ f ^ " " ^ static_string ^ " " ^ dynamic_string ^ ") " ^ body ^ ")"
		    end 
		val eqnsa = ListPair.map funcAToString (fnames,formals_envs)
	    in
		"(\n  " ^ (concatWith "\n  " eqnsa) ^ "\n)"
	    end
		;
	
	(* Create the abstract syntax tree of the annotated Scheme0 for program prg w.r.t. given division *)
        fun annotate prg divi = 
	let
	  
	  val (fnames:string list, ls) = prg;
	  val (formals_list:string list list, expr_list:SchAST.expr list) = ListPair.unzip ls

	  fun bt_lookup x divi h = 
		    let val (tau,_) = divi h in SchAST.lookup x (SchAST.lookup h fnames formals_list) tau end 

	  fun lift els = (map 
	    (fn (bt,e) => if bt = SchBTA.S then (Lift e) else e) 
	    els);

	  fun ann_expr (h:string, exp:SchAST.expr):(expra * SchBTA.bindtime) = (case exp of
	      (SchAST.Const c) => ((Const c), SchBTA.S)
	    | (SchAST.Variable x) => ((Variable x), (bt_lookup x divi h))
	    | (SchAST.If (e1, e2, e3)) => (let 
		val (e,v) = ann_list [e1,e2,e3] h;
		val bt = (hd v);
		val lifts = lift (ListPair.zip (v,e));
		val tup = ((hd e), (hd (tl e)), (hd (tl (tl e))));
	      in ((if bt = SchBTA.S 
		   then (IfS tup)
		   else (IfD tup)), (SchBTA.bt_lub v)) end)
	    | (SchAST.Op (str, els)) => (let
		val (e,v) = ann_list els h;
		val bt = SchBTA.bt_lub v;
	      in ((if bt = SchBTA.S 
		   then (OpS (str, e)) 
		   else (OpD (str, (lift (ListPair.zip (v,e)))))), bt) end)
	    | (SchAST.Call (f, els)) => (let
		val (e, v) = ann_list els h;
		val (div_f, div_r) = divi f;
	      in ((if (SchBTA.bt_lub div_f) = SchBTA.S 
		   then (CallS (f, e, []))
		   else let
		      fun filter_ann bt ls = (#2 (ListPair.unzip (List.filter 
			(fn (t,_) => (t = bt))
			(ListPair.zip (div_f, ls)))));
		      val s = filter_ann SchBTA.S e;
		      val d = (lift (filter_ann SchBTA.D (ListPair.zip (v,e))));
		   in (CallD (f, s, d)) end),
		   div_r)
	      end))
	  and ann_list ls h = ListPair.unzip (map (fn x => ann_expr (h,x)) ls);
	 
	  val (ann_bodies, _) = ListPair.unzip (map ann_expr (ListPair.zip (fnames, expr_list)));
	  
	  val ann_formals = map 
	    (fn (fname, formals) => let
		val (bt, _) = (divi fname);
		fun filter_bt t = 
		  #2
		  (ListPair.unzip
		  (List.filter
		  (fn (b,f) => b = t)
		  (ListPair.zip (bt, formals))))
		val s = filter_bt SchBTA.S;
		val d = filter_bt SchBTA.D;		
	      in
		(s, d)
	      end)
	    (ListPair.zip (fnames, formals_list));
	  
	  val ann_ls = map (fn ((a,b),c) => (a,b,c)) (ListPair.zip (ann_formals, ann_bodies));
	in
	   (fnames, ann_ls)
	end

	(* Specialize the given annotated program with values vs0 for static variables *)
	fun specialize prga vs0 = 
	    let 
	    
		val (fnames, ls) = prga;
	    
		(* Get a condensed string representation of a Scheme0 constant *)
		(* Used to create specialized function names *)
		fun constToString (SchAST.Numeral n) = Int.toString n
		  | constToString (SchAST.Boolean b) = if b then "#t" else "#f"
		  | constToString (SchAST.Symbol s) = s
		  | constToString SchAST.NIL = "NIL"
		  | constToString (SchAST.List (d1,d2)) = (list_helper (SchAST.List (d1,d2)))
		  | constToString (SchAST.Quote d) = "'" ^ (constToString d)
		and list_helper ls =
		    let
			fun unwrap SchAST.NIL = ["NIL"]
			  | unwrap (SchAST.List (a,b)) = (constToString a) :: (unwrap b)
			  | unwrap c = raise SchAST.SchError ("Error unwrapping list " ^ (constToString c))
		    in
			concatWith "." (unwrap ls)
		    end
		    ;
		fun make_new_name (f,vs) = if vs = [] then f else f ^ "-" ^ (concatWith ":" (List.map constToString vs));
		
		(*Looks up a function in f*)
		fun ann_lookup f = SchAST.lookup f fnames ls;
		
		
		fun reduce_func f vs = let
		  val (sformals, dformals, body) = ann_lookup f;
		
		  fun strip_const (SchAST.Const c) = c
		    | strip_const _ = raise SchAST.SchError("Expected a const!?");
		  
		  fun reduce exp = (case exp of
		      (Const c) => ((SchAST.Const c), [])
		    | (Variable x) => 
		      let
			val v = List.find (fn (a,_) => a = x) (ListPair.zip(sformals, vs));
		      in case v of
			  (Option.NONE) => ((SchAST.Variable x), [])
			| (Option.SOME (_,c)) => ((SchAST.Const c), [])
		      end
		    | (IfS (e1,e2,e3)) =>
		       let
			  val (cond:SchAST.expr, p1:pend_func list) = reduce e1;
		          val (res:SchAST.expr, p2:pend_func list) = (case (strip_const cond) of
			    (SchAST.Boolean true) => reduce e2
			  | (SchAST.Boolean false) => reduce e3
			  | _ => raise SchAST.SchError("THIS SHOULD NOT HAPPEN"));
		       in
			  (res, (List.concat [p1,p2]))
		       end
		    | (IfD (e1,e2,e3)) =>
		      let
			  val (re1, p1) = reduce e1;
			  val (re2, p2) = reduce e2;
			  val (re3, p3) = reduce e3;
		      in
			  ((SchAST.If (re1,re2,re3)), (List.concat [p1,p2,p3]))
		      end
		    | (CallS (fname, vs, vd)) => 
		      let
			val (svals, _) = ListPair.unzip (map reduce vs);
			val sconst = map strip_const svals;
		      in
			reduce_func fname sconst
		      end
		    | (CallD (fname, vs, vd)) =>
		      let
			val (dvals, dpending) = ListPair.unzip (map reduce vd);
			val (svals, _) = ListPair.unzip (map reduce vs);
			val sconst:SchAST.constant list = map strip_const svals;
			val nname:string = make_new_name (fname, sconst);
			val npend:pend_func = (fname, sconst);
			val ppend:pend_func list = List.concat dpending;
			val pend = npend::ppend;
		      in
			((SchAST.Call (nname, dvals)), pend)
		      end
		    | (OpS (prim, vs)) =>
		      let
			val (svals, _) = ListPair.unzip (map reduce vs);
			val sconst = map strip_const svals;
			val res = (SchAST.Const (SchAST.apply_prim prim sconst))
		      in
			(res, [])
		      end
		    | (OpD (prim, vd)) =>
		      let
			val (dvals, dpending) = ListPair.unzip (map reduce vd);
			val pend = List.concat dpending;
		      in
			((SchAST.Op (prim, dvals)), pend)
		      end
		    | (Lift exp) => reduce exp);
		 in
		    reduce body
		 end;
		
		fun complete pending marked = if pending = [] then ([], [])
		else let
		  val (f:string, vs:SchAST.constant list) = hd pending;
		  val name:string = make_new_name (f,vs);
		in
		if (List.exists (fn x => x = name) marked) then
		  complete (tl pending) marked
		else let
		  val (reduced:SchAST.expr, new_pending:pend_func list) = reduce_func f vs;
		  val (rnames:string list, rbodies:((string list * SchAST.expr) list)) = complete (List.concat [new_pending, (tl pending)]) (name::marked);
		  
		  val (_, dynargs:string list, _) = ann_lookup f;
		  
		  val final_names = name :: rnames;
		  val final_body = (dynargs, reduced);
		  val final_bodies = final_body::rbodies;
		  val final_prog:SchAST.program = (final_names, final_bodies);
		in
		  final_prog
		end
		end
		
		val goal_func:pend_func = ( (hd fnames), vs0 );
	    in
		complete [goal_func] []
	    end;

	fun pe prg env vs0 = specialize (annotate prg (SchBTA.bta prg env)) vs0;
end

