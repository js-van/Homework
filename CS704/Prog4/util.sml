structure SchUtil : SCHUTIL = struct
   exception SchError of string;
   fun concatWith s nil = ""
     | concatWith s (b::nil) = b
     | concatWith s (a::rest) = a ^ s ^ (concatWith s rest)
       ;

   fun get_name n = 
       let
	   val min_code = Char.ord #"a" 
	   val max_code = Char.ord #"z"
	   val num_codes = max_code-min_code
	   fun f acc n = 
	       if n > num_codes then
		   let
		       val code = (n mod num_codes) + min_code
		       val acc' = acc ^ (Char.toString (Char.chr code))
		   in
		       f acc' (n-num_codes)
		   end
	       else
		   acc ^ (Char.toString (Char.chr (n+min_code)))
       in
	   f "'" (n-1)
       end
end

