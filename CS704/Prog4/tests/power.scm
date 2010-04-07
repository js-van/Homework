(
  (define (power x n)
    (if (= n 0)
        1
	(* x (call power x (- n 1)))))
)
