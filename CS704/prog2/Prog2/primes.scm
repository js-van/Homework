(
  (define (getprimes n) (call take (call sieve (call from 2)) n) )

  (define (take ls i)
    (if (= i 0) NIL (cons (car ls) (call take (cdr ls) (- i 1)))))

  (define (from n) (cons n (call from (+ n 1))))

  (define (filterp ls p)
    (if (= (mod (car ls) p) 0)
	(call filterp (cdr ls) p)
	(cons (car ls) (call filterp (cdr ls) p))))

  (define (sieve ls) (cons (car ls)
    (call sieve (call filterp (cdr ls) (car ls)))))
)
