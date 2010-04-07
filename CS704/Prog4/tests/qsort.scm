(

(define (qsort l)
  (if (call lt (call len l) 2) 
    l
    (call helpqsort (call part (cdr l) (car l)) (car l) )))

(define (helpqsort p v)
  (call concat (call qsort (call p1 p)) (cons v (call qsort (call p2 p)))))

(define (len l)
  (if (null? l) 0
    (+ 1 (call len (cdr l)))))

(define (concat a b)
  (if (null? a) 
    b
    (cons (car a) (call concat (cdr a) b))))

(define (lt x y)
  (if (= y 0) #f
  (if (= x 0) #t
  (call helplt (div x 2) (div y 2) (mod x 2) (mod y 2) ))))

(define (helplt x y p q) 
  (if (= x y) 
    (if (= p 0) (= q 1) #f)
    (call lt x y)))

(define (mkpair a b)
  (cons a (cons b NIL)))

(define (p1 p) (car p))

(define (p2 p) (car (cdr p)))

(define (part l m)
  (if (null? l)
    (call mkpair NIL NIL)
    (call helppart (car l) (call part (cdr l) m) m)))
      
(define (helppart v p m)
  (if (call lt v m)
    (call mkpair (cons v (call p1 p)) (call p2 p))
    (call mkpair (call p1 p) (cons v (call p2 p)))))
)
