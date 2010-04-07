CM.make("./sources.cm");

fun list2scm [] = (SchAST.NIL)
  | list2scm l = (SchAST.List ((SchAST.Numeral (hd l)), (list2scm (tl l))))

fun test name interp warmer = let
  val _ = print ("-----------TESTING: " ^ name ^ "------------------------\n")
  
  fun dotest tfile args expected = let
      
      val filename = "tests/" ^ tfile;

      fun warming v = (warmer (SchTest.getProg filename) [v])
      val res = (warming (SchTest.test interp filename args)) (* handle x => #2 (print "Got exception", (SchAST.Numeral ~1)) *)
      val _ = (print ("Expected: " ^ (SchAST.constToString expected) ^ "\n"))
      val _ = if SchAST.constToString(res) = SchAST.constToString(expected) then
	  print "Pass\n"
	  else print ("!!!!!!!!!!!!!!!!!!!!!!!!!!FAIL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n    warmed = " ^ SchAST.constToString(res) ^ "\n")
    in
      (res, expected)
    end
  in
  [
    (dotest "div.scm" ([(SchAST.Numeral 6), (SchAST.Numeral 2)]) (SchAST.Numeral 3)),
    (dotest "div.scm" ([(SchAST.Numeral 6), (SchAST.Numeral 5)]) (SchAST.Numeral 1)),
    (dotest "div.scm" ([(SchAST.Numeral 6), (SchAST.Numeral 13)]) (SchAST.Numeral 0)),
    (dotest "div.scm" ([(SchAST.Numeral 0), (SchAST.Numeral 5)]) (SchAST.Numeral 0)),
    (dotest "div.scm" ([(SchAST.Numeral ~2), (SchAST.Numeral ~1)]) (SchAST.Numeral 2)),

    (dotest "mod.scm" ([(SchAST.Numeral 13), (SchAST.Numeral 10)]) (SchAST.Numeral 3)),
    (dotest "mod.scm" ([(SchAST.Numeral 1), (SchAST.Numeral 2)]) (SchAST.Numeral 1)),

    (dotest "gcd.scm" ([(SchAST.Numeral 13), (SchAST.Numeral 64)]) (SchAST.Numeral 1)),
    (dotest "gcd.scm" ([(SchAST.Numeral 26), (SchAST.Numeral 64)]) (SchAST.Numeral 2)),
    (dotest "gcd.scm" ([(SchAST.Numeral 105), (SchAST.Numeral 45)]) (SchAST.Numeral 15)),

    (dotest "lt.scm" ([(SchAST.Numeral 0), (SchAST.Numeral 1)]) (SchAST.Boolean true)),
    (dotest "lt.scm" ([(SchAST.Numeral 0), (SchAST.Numeral 0)]) (SchAST.Boolean false)),
    (dotest "lt.scm" ([(SchAST.Numeral 1), (SchAST.Numeral 0)]) (SchAST.Boolean false)),
    (dotest "lt.scm" ([(SchAST.Numeral 1), (SchAST.Numeral 1)]) (SchAST.Boolean false)),
    (dotest "lt.scm" ([(SchAST.Numeral 1371), (SchAST.Numeral 57818)]) (SchAST.Boolean true)),
    (dotest "lt.scm" ([(SchAST.Numeral 46464), (SchAST.Numeral 13745)]) (SchAST.Boolean false)),

    (dotest "fib.scm" ([(SchAST.Numeral 1)]) (SchAST.Numeral 1)),
    (dotest "fib.scm" ([(SchAST.Numeral 2)]) (SchAST.Numeral 2)),
    (dotest "fib.scm" ([(SchAST.Numeral 3)]) (SchAST.Numeral 3)),
    (dotest "fib.scm" ([(SchAST.Numeral 4)]) (SchAST.Numeral 5)),
    (dotest "fib.scm" ([(SchAST.Numeral 5)]) (SchAST.Numeral 8)),
    (dotest "fib.scm" ([(SchAST.Numeral 6)]) (SchAST.Numeral 13)),
    (dotest "fib.scm" ([(SchAST.Numeral 7)]) (SchAST.Numeral 21)),
    (dotest "fib.scm" ([(SchAST.Numeral 8)]) (SchAST.Numeral 34)),

    (dotest "len.scm" ([(list2scm [])]) (SchAST.Numeral 0)),
    (dotest "len.scm" ([(list2scm [1,2,3])]) (SchAST.Numeral 3)),

    (dotest "concat.scm" ([(list2scm [1, 2, 3, 4]), (list2scm [5, 6, 7, 8])]) (list2scm [1,2,3,4,5,6,7,8])),
    (dotest "concat.scm" ([(list2scm []), (list2scm [5, 6, 7, 8])]) (list2scm [5,6,7,8])),
    (dotest "concat.scm" ([(list2scm [1, 2, 3, 4]), (list2scm [])]) (list2scm [1,2,3,4])),
    
    (dotest "dotprod.scm" ([(list2scm [1, 0, 1]), (list2scm [3, 100, 5])]) (SchAST.Numeral 8)),

    (dotest "setappend.scm" ([(list2scm [1,2]), (list2scm [2, 3])]) (list2scm [1, 2, 3])),

    (dotest "qsort.scm" ([(list2scm [])]) (list2scm [])),
    (dotest "qsort.scm" ([(list2scm [1])]) (list2scm [1])),
    (dotest "qsort.scm" ([(list2scm [2, 1])]) (list2scm [1, 2])),
    (dotest "qsort.scm" ([(list2scm [1, 1])]) (list2scm [1, 1])),
    (dotest "qsort.scm" ([(list2scm [3,1,2])]) (list2scm [1,2,3])),
    (dotest "qsort.scm" ([(list2scm [2,1,3])]) (list2scm [1,2,3])),
    (dotest "qsort.scm" ([(list2scm [1,2,3])]) (list2scm [1,2,3])),
    (* This test is too slow in CBN for the normal unittest *)
     (dotest "qsort.scm" ([(list2scm [10, 3, 0, 4, 81, 9])]) (list2scm [0,3,4,9, 10,81])), 

    (dotest "find.scm" ([(list2scm [0, 57, 39, 1, 48, 48, 100]), (SchAST.Numeral 1)]) (SchAST.Numeral 3)),
    (dotest "find.scm" ([(list2scm [0, 57, 39, 1, 48, 48, 100]), (SchAST.Numeral 666)]) (SchAST.Numeral 7)),
    (dotest "find.scm" ([(list2scm [0, 57, 39, 1, 48, 48, 100]), (SchAST.Numeral 48)]) (SchAST.Numeral 4)),
    
    (dotest "primes.scm" ([(SchAST.Numeral 7)]) (list2scm [2,3,5,7,11,13,17])),

    (dotest "foo.scm" ([(SchAST.Numeral 1)]) (SchAST.Numeral 1)),

    (dotest "id.scm" ([(SchAST.Quote (SchAST.Numeral 1))]) (SchAST.Quote (SchAST.Numeral 1)))
  ]
  end

fun simple_warming prg vals = (hd vals);

fun go () = (#3 (use "setup.sml", CM.make("./sources.cm"), [  
    (*
   (test "Call-by-value" SchAST.cb_value  simple_warming),
   (test "Eager call-by-name" SchAST.eager_cb_name simple_warming),
    *)
   (test "Lazy call-by-name" SchAST.lazy_cb_name SchAST.cb_name_global_warming),
   (test "Call-by-need" SchAST.cb_need SchAST.cb_need_global_warming)
]))

fun chk() = use "setup.sml";

