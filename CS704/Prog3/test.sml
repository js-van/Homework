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

  fun test filename env vals = 
      let 
	  fun split_vals env vals = 
	      let 
		  val (spairs,dpairs) = List.partition (fn (bt,v) => bt = SchBTA.S) (ListPair.zip (env,vals))
	      in
		  (List.map #2 spairs, List.map #2 dpairs)
	      end
	  val _ = print("Testing file: " ^ filename ^ "\n")
	  val prg = getProg filename
	  val division = SchBTA.bta prg env
	  val f =
	      case prg of
		  ((f::_), _) => f
		| _ => raise SchAST.SchError "Ill-formed program"
	  val (tau,_) = division f
	  val (vs0,vd0) = split_vals tau vals
	  val prga = SchPE.annotate prg division
	  val prgs = SchPE.specialize prga vs0
	  val _ = print("Specialized Program:\n" ^ (SchAST.progToString prgs) ^ "\n")
	  val s_res = SchAST.interpret prgs vd0 
	  val res = SchAST.interpret prg vals
	  val oStream = TextIO.openOut(filename ^ ".res") 
      in
	  (TextIO.output(oStream, SchAST.constToString(res) ^ "\n");
	   TextIO.closeOut(oStream));
	  (division,prga,prgs,s_res,res)
      end
	  handle SchAST.SchError(s) => 
		 let val _ = print (s ^ "\n")
		 in (fn _ => (nil,SchBTA.S), (nil,nil),(nil,nil),(SchAST.Numeral(~1)),(SchAST.Numeral(~1))) end;
end;
