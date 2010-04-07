(
(define (concat a b)
  (if (null? a) 
    b
    (cons (car a) (call concat (cdr a) b))))
)