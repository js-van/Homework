structure SchUtil : SCHUTIL = struct
   fun concatWith s nil = ""
     | concatWith s (b::nil) = b
     | concatWith s (a::rest) = a ^ s ^ (concatWith s rest)
       ;
end

