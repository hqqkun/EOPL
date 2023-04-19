#lang eopl

(define end-cont
  (lambda ()
    (lambda (val)
       (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)))
)

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))))

(define list-sum/k
  (lambda (loi cont)
    (cond
      ((null? loi) (cont 0))
      (else 
        (list-sum/k (cdr loi)
          (lambda (rest-sum)
            (cont (+ (car loi) rest-sum)))))))
)

(display (list-sum '(1 2 3 4 5)))