(
(define (find l v)
  (if (null? l) 0
  (if (= (car l) v) 0
      (+ 1 (call find (cdr l) v)))))
)