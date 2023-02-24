#lang eopl

(define exists?
    (lambda (pred lst)
        (if (null? lst)
            #f
            (or 
                (pred (car lst)) 
                (exists? pred (cdr lst)))))
)

; test
(display  (exists? number? '(a b c 3 e)))