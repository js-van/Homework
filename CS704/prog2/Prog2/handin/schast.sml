(* Mikola Lysenko *)
structure SchAST : SCHEMEAST = struct

     datatype constant =
	      Numeral of int
	    | Boolean of bool
	    | Symbol of string
	    | NIL
	    | List of constant * constant
	    | Quote of constant
	    | ThunkConst of thunk
     and expr =
	 Const of constant
       | Variable of string
       | If of expr * expr * expr
       | Call of string * expr list
       | Op of string * expr list
     and thunk =
	 Thunk of expr * (string -> constant)
       | ListThunk of constant * thunk;

     (* Some type aliases for convenience *)	 
     type program = string list * (string list * expr) list;
     type environment = string -> constant;
     type interpreter = program -> constant list -> constant;
     type bound_intp = expr * environment -> constant;
     
     (* Use this to indicate errors, this is bottom *)
     exception SchError of string;
			     
     (* A convenience function used to print lists *)
     val concatWith = SchUtil.concatWith;
				 
     (* Get a string representation of a Scheme0 datum *)
     fun constToString (Numeral n) = Int.toString n
       | constToString (Boolean b) = if b then "#t" else "#f"
       | constToString (Symbol s) = s
       | constToString NIL = "NIL"
       | constToString (List (d1,d2)) = "(" ^ (list_helper (List (d1,d2))) ^ ")"
       | constToString (Quote d) = "(quote " ^ (constToString d) ^ ")"
       | constToString (ThunkConst t) = thunkToString t
     and list_helper ls =
	 let
	     fun unwrap NIL = nil
	       | unwrap (List (a,b)) = a :: (unwrap b)
	       | unwrap c = [c]  (* raise SchError ("Error unwrapping list " ^ (constToString c)) *)
	 in
	     concatWith " " (List.map constToString (unwrap ls))
	 end
     (* Get a string representation of a Scheme0 thunk *)
     and thunkToString (Thunk (e,_)) = "Thunk(" ^ (expToString e) ^ ")"
       | thunkToString (ListThunk (h,t)) = "ListThunk(" ^ (constToString h) ^ " . " ^ (thunkToString t) ^ ")"
     (* Get a string representation of a Scheme0 expression *)
     and expToString (Const c) = constToString c
       | expToString (Variable v) = v
       | expToString (If (e1,e2,e3)) = "(if " ^ (concatWith " " (List.map expToString [e1,e2,e3])) ^ ")"
       | expToString (Call (fname,elist)) = "(call " ^ fname ^ " " ^ (concatWith " " (List.map expToString elist)) ^ ")"
       | expToString (Op (op_name,elist)) = "(" ^ op_name ^ " " ^ (concatWith " " (List.map expToString elist)) ^ ")"
     (* Get a string representation of a Scheme0 program *)
     and progToString prg = 
	 let
	     fun funToString (f, (formals,body)) = 
		 "(define (" ^ f ^ " " ^ (concatWith " " formals) ^ ") " ^ (expToString body) ^ ")"
	 in
	     "(\n " ^ (concatWith "\n " (ListPair.map funToString prg)) ^ "\n)"
	 end;
    
    
    (*Interprets an expression in the given system *)
    fun mk_interpreter (
      prg:program,
      mk_env:(expr list * string list  * environment) -> (string -> constant), 
      mk_list:(expr * expr * environment) -> constant
    ):bound_intp = let

      (*Checks equality of constants*)
      fun const_eq (a:constant, b:constant):bool = case (a,b) of
	    ((Boolean x), (Boolean y)) => x = y
	  | ((Numeral x), (Numeral y)) => x = y
	  | ((Symbol x), (Symbol y)) => x = y
	  | ((Quote x), (Quote y)) => (const_eq (x, y))
	  | ((List (x1,x2)), (List (y1,y2))) => (const_eq (x1, y1)) andalso (const_eq (x2, y2))
	  | _ => raise (SchError "Unrecognized types for equality operator")

      and is_null c = (case c of
	  NIL => (Boolean true)
	| (List (_,_)) => (Boolean false)
	| (ThunkConst (ListThunk (_,_))) => (Boolean false)
	| _ => raise (SchError "Non-list type passed to null?"))
  
      (* Evaluates an operator *)
      and eval_op (opname:string, args, env):constant = let
	  fun eval arg = (eval_expr (arg, env))
	in case opname of
	  "car" =>  (case args of
		      arg::nil => (fn (List (x,y)) => x
				    | (ThunkConst (ListThunk (c,t))) => c
				    | _ => raise (SchError "Invalid arguments to car"))(eval arg)
		    | _ => raise (SchError "Invalid number of arguments to car"))
      | "cdr" =>  (case args of
		      arg::nil => (fn (List (x,y)) => y
				    | (ThunkConst (ListThunk (c,t))) => (thaw t)
				    | _ => raise (SchError "Invalid arguments to cdr"))(eval arg)
		    | _ => raise (SchError "Invalid number of arguments to cdr"))
      | "cons" =>  (case args of
		      a::b::nil => mk_list (a, b, env)
		    | _ => raise (SchError "Invalid number of arguments to cons"))
      | "null?" => (case args of
		      a::nil => (is_null (eval a))
		    | _ => raise (SchError "Invalid number of arguments to null?"))
      | "+" =>   (case args of
		      a::b::nil => (fn (Numeral x, Numeral y) => (Numeral (x+y))
				    | _ => raise (SchError "Invalid type for arguments to +"))((eval a),(eval b))
		    | _ => raise (SchError "Invalid number of arguments to +"))
      | "-" => (case args of
		    a::b::nil => (fn (Numeral x, Numeral y) => (Numeral (x-y))
				  | _ => raise (SchError "Invalid type for arguments to -"))((eval a),(eval b))
		  | _ => raise (SchError "Invalid number of arguments to -"))
      | "*" => (case args of
				    a::b::nil => (fn (Numeral x, Numeral y) => (Numeral (x*y))
						  | _ => raise (SchError "Invalid type for arguments to *"))((eval a),(eval b))
				  | _ => raise (SchError "Invalid number of arguments to *"))
      | "div" => (case args of
				    a::b::nil => (fn (Numeral x, Numeral y) => (Numeral (x div y))
						  | _ => raise (SchError "Invalid type for arguments to div"))((eval a),(eval b))
				  | _ => raise (SchError "Invalid number of arguments to div"))
      | "mod" =>    (case args of
				    a::b::nil => (fn (Numeral x, Numeral y) => (Numeral (x mod y))
						  | _ => raise (SchError "Invalid type for arguments to mod"))((eval a),(eval b))
				  | _ => raise (SchError "Invalid number of arguments to mod"))
      | "=" =>    (case args of a::b::nil => (fn (x:constant, y:constant) => (Boolean (const_eq (x, y))))((eval a), (eval b))
		      | _ => raise (SchError "Invalid number of arguments to cons"))
      | x => raise (SchError ("Unknown operator "^ x))
      end
    
      (* Thaws a thunk out *)
      and thaw (t:thunk):constant = (case t of
	  (Thunk (expr, env)) => (eval_expr (expr, env))
	| (ListThunk (_, _)) => (ThunkConst t))

      (*Evaluates a complete expression*)
      and eval_expr (exp:expr, env:environment):constant = (case exp of
	  (Const c) => (case c of
	    (ThunkConst t) => (thaw t)
	  | _ => c)
	| (Variable str) => (case (env str) of
	      (ThunkConst t) => (thaw t)
	    | x => x)
	| (Call (fname, args)) => (eval_call (fname, args, env))
	| (Op (opname, args)) => (eval_op (opname, args, env))
	| (If (pred, t, f)) => (case (eval_expr (pred, env)) of
	      (Boolean x) => if x then (eval_expr (t, env)) 
				  else (eval_expr (f, env))
	    | _ => raise (SchError "Non-boolean expression in predicate for IF")))

      (* Extracts data associated to a procedure from the program *)
      and locate_proc (fname:string):(string list * expr) = 
	case (List.filter (fn (f,_) => f = fname) (ListPair.zip prg) ) of
	    nil => raise (SchError ("No such procedure " ^ fname))
	  | head::nil => (#2 head)
	  | head::_ => raise (SchError ("Multiple definitions of procedure " ^ fname))
  
      (* Evaluates a function call *)
      and eval_call (fname:string, args:expr list, env:environment):constant = let
	val proc = (locate_proc fname);
	in
	  eval_expr ((#2 proc), (mk_env (args, (#1 proc), env)))
	end
    in
      eval_expr
    end

    (*Interprets a program *)
    fun interpret_prog (
      prg:program,
      vals:constant list, 
      eval_expr:bound_intp,
      mk_env:(expr list * string list * environment) -> (string -> constant)
    ): constant = let
      (* Initial conditions *)
      val def_call = (hd (#1 prg))
      val def_env = (mk_env ([], [], (fn _ => raise (SchError "ERROR"))))
      val def_args = map (fn x => (Const x)) vals

    in
      eval_expr ((Call (def_call, def_args)), def_env)
    end

    (* A call-by-value interpreter with eager list primitives *)
    fun cb_value prg vals = let

      fun mk_env (args, fnames, env) = let
	fun eval arg = eval_expr (arg, env)
	val eargs = ListPair.zip (fnames, (map eval args))
      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (#2 x))
      end
      and mk_list (a, b, env) = (List ((eval_expr (a, env)), (eval_expr (b, env))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)

    in
      interpret_prog (prg, vals, eval_expr, mk_env)
    end

    (* A call-by-name interpreter with eager list primitives *)      
    fun eager_cb_name prg vals = let

      fun mk_env (args, fnames, env) = let
	fun freeze exp = case exp of
	  (Const c) => c
	| _ => (ThunkConst (Thunk (exp, env)))
	val eargs = ListPair.zip (fnames, (map freeze args))
      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (#2 x))
      end

      and mk_list (a, b, env) = (List ((eval_expr (a, env)), (eval_expr (b, env))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)

    in
      interpret_prog (prg, vals, eval_expr, mk_env)
    end

    (* A call-by-name interpreter with lazy list primitives *)
    fun lazy_cb_name prg vals = let

      fun mk_env (args, fnames, env) = let
	fun freeze exp = case exp of
	  (Const c) => c
	| _ => (ThunkConst (Thunk (exp, env)))
	val eargs = ListPair.zip (fnames, (map freeze args))
      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (#2 x))
      end

      and mk_list (a, b, env) = (ThunkConst (ListThunk ((eval_expr (a, env)), (Thunk (b, env)))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)
    in
      interpret_prog (prg, vals, eval_expr, mk_env)
    end

    (* Global warming function for call-by-name (no updating env)*)
    fun cb_name_global_warming prg vals = let
      fun mk_env (args, fnames, env) = let
	fun freeze exp = case exp of
	  (Const c) => c
	| _ => (ThunkConst (Thunk (exp, env)))
	val eargs = ListPair.zip (fnames, (map freeze args))
      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (#2 x))
      end

      and mk_list (a, b, env) = (ThunkConst (ListThunk ((eval_expr (a, env)), (Thunk (b, env)))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)
      and thaw (t:thunk):constant = (case t of
	  Thunk (exp, env) => (thaw_const (eval_expr (exp, env)))
	| ListThunk (c, t) => (List (c, (thaw t))))
      and thaw_const c:constant = (case c of 
	  (ThunkConst t) => thaw t
	| (List (a, b)) => (List ((thaw_const a), (thaw_const b)))
	| _ => c)
    in
      thaw_const (hd vals)
    end


    (* A call-by-need interpreter with lazy list primitives *)
    (*fun cb_need prg vals = (Numeral ~1);*)
    fun cb_need prg vals = let

      fun mk_env (args, fnames, env) = let
	fun extract r = (case (!r) of
	    (Const c) => c
	  | _ => (let
		    val v = eval_expr (!r, env);
	         in
		   ( r := (Const v); v )
		 end));

	val eargs = ListPair.zip (fnames, (map ref args))

      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (extract (#2 x)))
      end

      and mk_list (a, b, env) = (ThunkConst (ListThunk ((eval_expr (a, env)), (Thunk (b, env)))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)
    in
      interpret_prog (prg, vals, eval_expr, mk_env)
    end

    (* Global warming function for call-by-need (updating env)*)
    (* CHANGEME: Currently returns -1 *)
    fun cb_need_global_warming prg vals = let
      fun mk_env (args, fnames, env) = let
	fun extract r = (case (!r) of
	    (Const c) => c
	  | _ => (let
		    val v = eval_expr (!r, env);
	         in
		   ( r := (Const v); v )
		 end));

	val eargs = ListPair.zip (fnames, (map ref args))

      in
	fn str => (case (List.find (fn (x,y) => x = str) eargs) of
	   NONE => raise (SchError "Invalid argument")
	 | (SOME x) => (extract (#2 x)))
      end

      and mk_list (a, b, env) = (ThunkConst (ListThunk ((eval_expr (a, env)), (Thunk (b, env)))))
      and eval_expr (expr, env) = (mk_interpreter (prg, mk_env, mk_list)) (expr, env)

      and thaw (t:thunk):constant = (case t of
	  (Thunk (exp, env)) => thaw_const (eval_expr (exp,env))
	| (ListThunk (c, v)) => (List ((thaw_const c), (thaw v))))

      and thaw_const c:constant = (case c of
	  (ThunkConst t) => thaw t
	| (List (a,b)) => (List ((thaw_const a), (thaw_const b)))
	| _ => c)
    in
      thaw_const (hd vals)
    end
end

