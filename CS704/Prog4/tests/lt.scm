(

(define (lt x y)
  (if (= y 0) #f
  (if (= x 0) #t
  (call helplt (div x 2) (div y 2) (mod x 2) (mod y 2) ))))

(define (helplt x y p q) 
  (if (= x y) 
    (if (= p 0) (= q 1) #f)
    (call lt x y)))
)