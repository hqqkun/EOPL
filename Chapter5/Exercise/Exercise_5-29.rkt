#lang eopl

(define n 'uninitialized)
(define a 'uninitialized)

(define fact-iter
  (lambda (x)
    (set! n x)
    (set! a 1)
    (fact-iter-acc))
)

(define fact-iter-acc
  (lambda ()
    (if (zero? n)
      a
      (begin
        (set! a (* a n))
        (set! n (- n 1))
        (fact-iter-acc))))
)

(display (fact-iter 6))