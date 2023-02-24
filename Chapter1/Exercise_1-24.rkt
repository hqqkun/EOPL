#lang eopl

(define every?
    (lambda (pred lst)
        (if (null? lst)
            #t
            (and 
                (pred (car lst)) 
                (every? pred (cdr lst)))))
)

; test
(display (every? number? '(a b c 3 e)))