#lang eopl

(define up
    (lambda (lst)
        (cond 
            ((null? lst) '())
            ((list? (car lst)) (append (car lst) (up (cdr lst))))
            (else (cons 
                    (car lst)
                    (up (cdr lst))))))
)