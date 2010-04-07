(* constructing parser *)
structure SchemeLrVals =
	SchemeLrValsFun(structure Token = LrParser.Token);
structure SchemeLex =
	SchemeLexFun(structure Tokens = SchemeLrVals.Tokens);
structure SchemeParser =
	Join(structure ParserData = SchemeLrVals.ParserData
				structure Lex = SchemeLex
				structure LrParser = LrParser);

structure SchTest : SCHTEST = struct

    fun makeStream fileName = 
      let 
	  val inStream = TextIO.openIn(fileName)
	  fun getString numChars = Option.getOpt(TextIO.inputLine(inStream), "")
      in
	  (SchemeParser.makeLexer (getString), inStream)
      end
		  
	  
  val invoke = fn lexstream =>
		  let 

		      val print_error = fn(s, i:int, _) =>
					  print("Error, line " ^ Int.toString(i) ^ ", " ^ s ^ "\n")
		  in 
		      SchemeParser.parse(15, lexstream, print_error, ())
		  end
		      
  fun getProg fileName =
      let
	  val (tokenStream, fileStream) = makeStream fileName
	  val (prog,_) = invoke tokenStream
      in
	  TextIO.closeIn(fileStream);
	  prog
      end
	  ;
	  	  
  fun test filename = 
      let
	  val prg = getProg filename
	  val t = SchAST.type_check prg
      in
	  case t of
	      NONE => let val _ = print "FAIL\n" in t end
	    | SOME (typ,_) => let val _ = print ((LamAST.typToString typ) ^ "\n") in t end
      end
      	  handle SchUtil.SchError(s) => 
		 let val _ = print (s ^ "\n")
		 in NONE end
end;
