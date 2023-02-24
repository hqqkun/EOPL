#lang eopl

; we just get the list after s
(define remove-first
    (lambda (s los)
        (cond
            ((null? los) '())
            ((eqv? (car los) s) (cdr los))
            (else (remove-first s (cdr los)))))
)

; test
(define los '(1 2 3 4 5 6 7))
(display (remove-first 4 los))
