((define (fib n) 
  (if (= n 0) 1 
  (if (= n 1) 1
    (+ (call fib (- n 1)) (call fib (- n 2)))))
))