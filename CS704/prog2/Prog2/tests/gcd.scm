((define (gcd a b)
  (if (= a 0) b
    (call gcd (mod b a) a))))