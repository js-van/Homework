signature SCHEMEAST = sig

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

    type program = string list * (string list * expr) list;

    exception SchError of string;

    val constToString : constant -> string;

    val expToString : expr -> string;

    val progToString : program -> string;

    val interpret : program -> constant list -> constant;

    val lookup : string -> string list -> 'a list -> 'a;

    val apply_prim : string -> constant list -> constant;
end
