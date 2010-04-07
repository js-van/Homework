signature SCHEMEAST = sig

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

    type program = string list * (string list * expr) list;
    type environment = string -> constant;
    type interpreter = program -> constant list -> constant;

    exception SchError of string;

    val constToString : constant -> string;

    val expToString : expr -> string;

    val progToString : program -> string;

    val cb_value : interpreter;

    val eager_cb_name : interpreter;

    val lazy_cb_name : interpreter;
	
    val cb_need : interpreter;

    val cb_need_global_warming : interpreter;
    val cb_name_global_warming : interpreter;
end
