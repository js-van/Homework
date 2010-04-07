((define (len l)
  (if (null? l) 0
    (+ 1 (call len (cdr l))))))