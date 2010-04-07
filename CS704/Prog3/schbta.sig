signature SCHEMEBTA = sig

	datatype bindtime = S | D;

	type btenv = bindtime list * bindtime;

	type division = string -> btenv;

	val divisionToString : SchAST.program -> division -> string;

	val bt_lub : bindtime list -> bindtime;

	val bta : SchAST.program -> bindtime list -> division;
	    
end
