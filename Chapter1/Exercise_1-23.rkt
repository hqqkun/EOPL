#lang eopl

(define list-index
    (lambda (pred lst)
        (letrec 
            ([L (lambda (lst index)
                    (cond
                        ((null? lst) #f)
                        ((pred (car lst)) index)
                        (else (L (cdr lst) (+ index 1)))))])
        (L lst 0)))
)

; test

(display (list-index number? '(a 2 (1 3) b 7)))
(newline)
(display  (list-index symbol? '(a (b c) 17 foo)))
(newline)
(display  (list-index symbol? '(1 2 (a b) 3)))