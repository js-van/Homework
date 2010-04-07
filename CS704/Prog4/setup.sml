CM.make("./sources.cm");

fun chk() = use "setup.sml";

fun test_lamast f =
let
  val filename = "tests/" ^ f;

  val _ = print ("\n\nTesting " ^ filename ^ "\n\n");

  val prog = SchTest.getProg filename;
  val progstr = SchAST.progToString prog;
  val lam_ast = SchAST.toLamExp prog;
  val lamstr = LamAST.expToString lam_ast;
  val _ = print ("Got prog:\n" ^ progstr ^
	 "\nLamAST:\n" ^ lamstr ^ "\n");
  val _ = SchTest.test filename;
in
    ()
end


fun go() = 
(
  (test_lamast "id.scm");
  (test_lamast "div.scm");
  (test_lamast "name_test.scm");
  (test_lamast "cons.scm");
  (test_lamast "fib.scm");
  (test_lamast "concat.scm");
  (test_lamast "if.scm");
  (test_lamast "qsort.scm");
  (test_lamast "nil.scm");
  (test_lamast "iffail.scm");
  (test_lamast "find.scm");
  (test_lamast "len.scm");
  (test_lamast "lt.scm");
  (test_lamast "mod.scm");
  (test_lamast "primes.scm");
  (test_lamast "power.scm");
  (test_lamast "setappend.scm");

  print "Done."
);

