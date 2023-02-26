#lang eopl

; 
(define list-set
    (lambda (lst n x)
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))
        ))
)

; test
; (define lst '(a b c d))
; (display (list-set '(a b c d) 2 '(1 2)))
(display (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3))