#lang eopl

(define remove
    (lambda (s los)
        (letrec ([R (lambda (los) 
                        (cond
                            ((null? los) '())
                            ((eqv? (car los) s) (R (cdr los)))
                            (else (cons (car los) (R (cdr los))))))])
            (R los)))
)

; test
(define los '(1 4 5 6 8 7 4 2 5 4))
(display (remove 4 los))