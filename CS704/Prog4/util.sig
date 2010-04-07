signature SCHUTIL = sig
    exception SchError of string;
    val concatWith: string -> string list -> string;
    val get_name: int -> string;
end
