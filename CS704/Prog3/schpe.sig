signature SCHEMEPE = sig
	
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

	val exprAToString : expra -> string;

	val progAToString : programa -> string;

	val annotate : SchAST.program -> SchBTA.division -> programa;

	val specialize : programa -> SchAST.constant list -> SchAST.program;

	val pe : SchAST.program -> SchBTA.bindtime list -> SchAST.constant list -> SchAST.program;

end
