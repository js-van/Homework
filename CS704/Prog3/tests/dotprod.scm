((define (dotprod v1 v2)
  (if (null? v1) 0
    (+ (* (car v1) (car v2)) (call dotprod (cdr v1) (cdr v2))))))