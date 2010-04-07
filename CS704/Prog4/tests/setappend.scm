((define (setappend ls1 ls2)
   (if (null? ls1)
       ls2
       (if (call ismember (car ls1) ls2)
           (call setappend (cdr ls1) ls2)
           (cons (car ls1) (call setappend (cdr ls1) ls2)))))
 (define (ismember x ls)
   (if (null? ls)
       #f
       (if (= x (car ls)) #t (call ismember x (cdr ls))))))