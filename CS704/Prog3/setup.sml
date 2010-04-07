CM.make("./sources.cm");

fun chk() = use "setup.sml";

fun list2scm [] = (SchAST.NIL)
  | list2scm l = (SchAST.List ((SchAST.Numeral (hd l)), (list2scm (tl l))))

fun list2String ls = "[" ^ (foldl (fn (s, r) => if r = "" then s else (s ^ "," ^ r)) "" ls) ^ "]"

fun bindList2String bindings = let
  fun bind2str SchBTA.S = "S"
    | bind2str SchBTA.D = "D";
in
    list2String (map bind2str bindings)
end

fun constList2String constants = list2String (map SchAST.constToString constants);

fun test_pe fname bindings constants expect_div expect_ann expect_pe = let
  val filename = "tests/" ^ fname;
  val _ = print ("Testing PE for " ^ fname ^ 
      "\nGiven bindings: " ^ (bindList2String bindings) ^ 
      "\nConstants: " ^ (constList2String constants) ^ "\n");

  (* Read program *)
  val prog = SchTest.getProg filename;
  val progstr = SchAST.progToString prog
  val _ = print ("Program:\n" ^ progstr);

  (* Test BTA *)
  val divi = SchBTA.bta prog bindings;
  val div_str = (SchBTA.divisionToString prog divi);
  val _ = print ("Expected Div:\n" ^ expect_div ^ "\n" ^
		 "Division:\n" ^ div_str ^ "\n");
  val pass_div = (expect_div = div_str);
  

  (* Test annotation *)
  val ann = SchPE.annotate prog divi;
  val ann_str = SchPE.progAToString ann;
  val _ = print ("Expected Ann:\n" ^ expect_ann ^ "\n" ^
		 "Annotation:\n" ^ ann_str ^ "\n");
  val pass_ann = (expect_ann = ann_str);
  
  (* Test partial evaluation *)
  val pe = SchPE.specialize ann constants;
  val pe_str = SchAST.progToString pe;
  val _ = print ("Expected PE:\n" ^ expect_pe ^ "\n" ^
		 "PE:\n" ^ pe_str ^ "\n");
  val pass_pe = (expect_pe = pe_str);


  (*Check for conformance *)
  val pass_total = (pass_div andalso pass_ann andalso pass_pe);
  val _ = if pass_total then print "Pass\n"
	  else print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!FAIL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
in
    (pass_div, pass_ann, pass_pe)
end

  
fun go() = let
  val _ = print ("Running tests...\n")
  val tests = [
    (test_pe "id.scm" [SchBTA.S] [(SchAST.Numeral 1)] "" "" ""),
    (test_pe "id.scm" [SchBTA.D] [] "" "" ""),

    (test_pe "power.scm" [SchBTA.D, SchBTA.S] [(SchAST.Numeral 3)] "" "" ""),

    (test_pe "qsort.scm" [SchBTA.S] [(list2scm [3,1,5,7,2])] "" "" ""),


    (test_pe "id.scm" [SchBTA.S] [(SchAST.Numeral 1)] "" "" "")
    
  ];

in
  print "Done."
end



