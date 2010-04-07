signature LAMAST = sig
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

    val expToString: exp -> string;
    val typToString: lamtype -> string;
    val tc: exp -> (lamtype * subst) option;
end
