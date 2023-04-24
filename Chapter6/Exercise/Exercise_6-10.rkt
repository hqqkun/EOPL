#lang eopl

(define list-sum
  (lambda (loi)
    (list-sum/k loi 0)))

(define list-sum/k
  (lambda (loi cont)
    (cond
      ((null? loi) cont)
      (else 
        (list-sum/k (cdr loi) (+ cont (car loi))))))
)

(display (list-sum '(1 2 3 4 5)))