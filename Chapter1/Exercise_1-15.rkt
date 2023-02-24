#lang eopl

(define duple 
    (lambda (n x)
        (letrec ([D (lambda (n)
                        (if (zero? n) '() (cons x (D (- n 1)))))])
            (D n)))
)

; test
(display (duple 2 'x))