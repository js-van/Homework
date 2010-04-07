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
		      	  
  fun printResult filename c =
      let 
	  val oStream = TextIO.openOut(filename)
      in
	  (TextIO.output(oStream, SchAST.constToString(c) ^ "\n");
	   TextIO.closeOut(oStream))
      end
	  
  fun test (intp:SchAST.interpreter) filename vals =
      let 
	  val _ = print("Testing file: " ^ filename ^ "\n");
	  val prg = getProg filename;
	  val result = intp prg vals
	  val _ = print ("Result: " ^ (SchAST.constToString result) ^ "\n");
      in
	  result
      end
	  handle SchAST.SchError(s) => let val _ = print s in (SchAST.Numeral ~1) end;


end





