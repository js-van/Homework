(
(define (testcons2 a b)
  (cons (cons (car a) (call testcons2 (cdr a) b)) b))
)