structure SchAST : SCHEMEAST = struct

     datatype constant =
	      Numeral of int
	    | Boolean of bool
	    | Symbol of string
	    | NIL
	    | List of constant * constant
	    | Quote of constant
     and expr =
	 Const of constant
       | Variable of string
       | If of expr * expr * expr
       | Call of string * expr list
       | Op of string * expr list

     (* Some type aliases for convenience *)	 
     type program = string list * (string list * expr) list;

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
     and list_helper ls =
	 let
	     fun unwrap NIL = nil
	       | unwrap (List (a,b)) = a :: (unwrap b)
	       | unwrap c = raise SchError ("Error unwrapping list " ^ (constToString c))
	 in
	     concatWith " " (List.map constToString (unwrap ls))
	 end
     (* Get a string representation of a Scheme0 expression *)
     and expToString (Const c) = constToString c
       | expToString (Variable v) = v
       | expToString (If (e1,e2,e3)) = "(if " ^ (concatWith " " (List.map expToString [e1,e2,e3])) ^ ")"
       | expToString (Call (fname,elist)) = "(call " ^ fname ^ " " ^ (concatWith " " (List.map expToString elist)) ^ ")"
       | expToString (Op (op_name,elist)) = "(" ^ op_name ^ " " ^ (concatWith " " (List.map expToString elist)) ^ ")"
     (* Get a string representation of a Scheme0 program *)
     and progToString prg = 
	 let
	     fun funToString (f, (formals, exp)) =
		 let
		     val formals_string = concatWith " " formals
		     val body = (expToString exp)
		 in
		     case formals of
			 nil => "(define (" ^ f ^ ")\n    " ^ body ^ ")"
		       | _ => "(define (" ^ f ^ " " ^ formals_string ^ ")\n    " ^ body ^ ")"
		 end 
	     val eqns = ListPair.map funToString prg
	    in
		"(\n  " ^ (concatWith "\n  " eqns) ^ "\n)"
	    end
		;

    (* Apply primitive operation prim to args *)
    fun apply_prim prim args =
	case (prim,args) of
	    ("+",[Numeral(a),Numeral(b)]) => (Numeral (a+b))
	  | ("-",[Numeral(a),Numeral(b)]) => (Numeral(a-b))
	  | ("*",[Numeral(a),Numeral(b)]) => (Numeral(a*b))
	  | ("div",[Numeral(a),Numeral(b)]) => (Numeral(a div b))
	  | ("mod",[Numeral(a),Numeral(b)]) => (Numeral(a mod b))
	  | ("=",[Numeral(a),Numeral(b)]) => Boolean (a=b)
	  | ("cons",[a,NIL]) => (List(a,NIL))
	  | ("cons",[a,List(b,c)]) => (List(a,List(b,c)))
	  | ("car",[List(a,_)]) => a
	  | ("cdr",[List(_,b)]) => b
	  | ("null?",[NIL]) => Boolean(true)
	  | ("null?",[_]) => Boolean(false)
	  | (_,_) => 
	    raise SchError("Error: error applying " ^ prim ^ " to " ^ (concatWith " " (List.map constToString args)) ^ "\n")
	;

    (* Lookup value for key x in parallel list environment (used for environment and program lookup) *)
    fun lookup x (k::keys) (v::vals) =
	 if (x = k) then v else lookup x keys vals
       | lookup x _ _ = raise SchError("Unknown id " ^ x ^ " referenced\n")
	 ;


    (* Interpret Scheme0 program by passing values to goal function *)
    fun interpret (prg:program) (vals:constant list) : constant =
	let
	    fun eval xs vs (Const (Quote d)) = d
	      | eval xs vs (Const c) = c
	      | eval xs vs (Variable x) = lookup x xs vs
	      | eval xs vs (If (c,t,f)) =
		let
		    val cond = eval xs vs c
		in
		    case cond of
			Boolean b => if b then eval xs vs t else eval xs vs f
		      | c => raise SchError("If condition is not boolean: " ^ constToString(c) ^ "\n")
		end
	      | eval xs vs (Call (f,actuals)) =
		let
		    val (fnames,defs) = prg
		    val (formals,body) = lookup f fnames defs
		    val actuals = List.map (eval xs vs) actuals
		in
		    eval formals actuals body
		end
	      | eval xs vs (Op (prim,args)) = 
		let
		    val args = List.map (eval xs vs) args
		in
		    apply_prim prim args
		end
	    val (_,defs) = prg
	    val (formals,body) = hd defs
	in
	    eval formals vals body
	end


end

