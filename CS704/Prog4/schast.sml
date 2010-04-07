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

    (* Node in a call graph (function_name, list of fns that call this fn, list of fns called by this fn) *)
    datatype gnode = GNode of string * string list * string list;
    fun get_name (GNode (name,parents,children)) = name; (* Function name *)
    fun get_parents (GNode (name,parents,children)) = parents; (* Functions that call this fn *)
    fun get_children (GNode (name,parents,children)) = children; (* Functions called by this fn *)

    (* Get string representation of a call graph *)
    fun callGraphToString graph = 
	let
	    fun gnodeToString gnode =
		let
		    val str = "gnode(" ^ (get_name gnode) ^ ", parents: [" ^ (concatWith "," (get_parents gnode)) ^ "], "
		in 
		    str ^ "children: [" ^ (concatWith "," (get_children gnode)) ^ "])"
		end
	in
	    (concatWith "\n" (List.map gnodeToString graph)) ^ "\n"
	end

    (* Make call graph *)
    fun make_call_graph fnames exp_list =
	let 
	    fun nodups names = 
		List.foldl (fn (g,acc) => if List.exists (fn name => (name = g)) acc then acc else g::acc) nil names
	    (* Find functions called in an expression *)
	    fun get_children_aux (Call (f,actuals)) = f::(List.concat (List.map get_children_aux actuals))
	      | get_children_aux (Op (prim,actuals)) = List.concat (List.map get_children_aux actuals)
	      | get_children_aux (If (e1,e2,e3)) = List.concat (List.map get_children_aux [e1,e2,e3])
	      | get_children_aux _ = []
	    and get_children exp = nodups (get_children_aux exp)
	    fun get_parents f fnames exp_list =
		let
		    fun func_filter (_,exp) = List.exists (fn h=>(f=h)) (get_children exp)
		    val ls = List.filter func_filter (ListPair.zip (fnames,exp_list))
		    val (pnames,pexp) = ListPair.unzip ls
		in
		    pnames
		end
	    (* Create a node for function f with body exp *)
	    fun make_node fnames exp_list (f,exp) =
		let
		    val children = get_children exp 
		    val parents = get_parents f fnames exp_list
		in
		    GNode(f,parents,children)
		end
	in
	   List.map (make_node fnames exp_list) (ListPair.zip(fnames,exp_list))
	end
	    ;

    (* Translate constants to dummy variables *)
    fun do_const c = (case c of
        (Numeral _) => (LamAST.Var "_int")
      | (Boolean _) => (LamAST.Var "_bool")
      | (Symbol _) => (LamAST.Var "_symbol")
      | NIL => (LamAST.Var "_nil")
      | (List (a, d)) => do_exp (Op ("cons", [(Const a),(Const d)]))
      | (Quote c) => do_const c)

    (* Translate expression to corresponding lambda expression *)		and do_exp e = (case e of
        (Const c) => do_const c
      | (Variable x) => (LamAST.Var x)
      | (If (e1,e2,e3)) => (LamAST.If ((do_exp e1), (do_exp e2), (do_exp e3)))
      | (Call (f, els)) => (LamAST.Ap ((LamAST.Var f), (map do_exp els)))
      | (Op (prim, els)) => (LamAST.Ap ((LamAST.Var prim), (map do_exp els))))



    (* Make a lambda expression for a function definition *)
    and do_function prg f = 
      let
	val SOME (_, (formals, body)) = List.find (fn (s,_) => s = f) (ListPair.zip prg);
	val res:LamAST.exp = (LamAST.Lambda (formals, (do_exp body)));
      in
	res
      end;

     (* remove nodes for functions listed in names from call_graph *)
     fun remove_nodes names call_graph =
	 let
   	     fun is_name_to_remove f = List.exists (fn g=> f = g) names
	     fun is_node_to_remove node = is_name_to_remove (get_name node)
	     val new_graph = List.filter (fn node=> not (is_node_to_remove node)) call_graph
	     fun prune_lists node =
		 let
		     val new_parents = List.filter (fn parent => not (is_name_to_remove parent)) (get_parents node)
		     val new_children = List.filter (fn child => not (is_name_to_remove child)) (get_children node)
		 in
		     GNode(get_name node,new_parents,new_children)
		 end
	 in
	     List.map prune_lists new_graph
	 end
     fun root_filter node = null (get_parents node)
     fun leaf_filter node = null (get_children node)
     fun interior_filter node = (not (root_filter node)) andalso (not (leaf_filter node))
     fun filter_graph filter call_graph =
	 List.map get_name (List.filter filter call_graph)
	 
    (* Creates the next let or letrec expression *)
    fun make_next_level goal prg nil = LamAST.Var(goal) (* call graph is empty now, return reference to goal function *)
      | make_next_level goal prg call_graph =
	let
            (* Find functions that are roots in the current call-graph (are not called) *)
	    val roots = filter_graph root_filter call_graph 
	    val interior = filter_graph interior_filter call_graph (* are both called and make calls *)
	    val leaves = filter_graph leaf_filter call_graph (* do not make calls *)
	    val (constr,chosen) = 
		case (leaves,interior) of
		    (nil,nil) => (LamAST.Let,roots) (* choose roots if no leaves or interior nodes *)
		  | (nil,_) => (LamAST.Letrec,interior) (* choose interior nodes if no leaves *)
		  | _ => (LamAST.Let,leaves) (* choose leaves first *)
	    val lams = List.map (do_function prg) chosen
	    val new_graph = remove_nodes chosen call_graph
	in
	    (* Create let or letrec, call recursively for body expression *)
	    constr (chosen,lams,make_next_level goal prg new_graph)
	end
	    ;

    (* Convert Scheme0 program to intermediate lambda-calc-like language *)
    fun toLamExp prg = 
	let
	    val (fnames,defs) = prg
	    val (formals_list,exp_list) = ListPair.unzip defs
	    val call_graph = make_call_graph fnames exp_list (* Make a call-graph of functions *)
	in
	    make_next_level (hd fnames) prg call_graph (* Begin program translation *)
	end
		
    (* type check program *)
    fun type_check prg = LamAST.tc (toLamExp prg);
end

