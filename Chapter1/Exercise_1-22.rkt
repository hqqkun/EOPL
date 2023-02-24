#lang eopl

(define filter-in
    (lambda (pred lst)
        (letrec 
            ([F (lambda (lst)
                    (cond
                        ((null? lst) '())
                        ((pred (car lst)) (cons (car lst) (F (cdr lst))))
                        (else (F (cdr lst)))))])
            (F lst)))
)

; test
(display  (filter-in number? '(a 2 (1 3) b 7)))
(display  (filter-in symbol? '(a (b c) 17 foo)))