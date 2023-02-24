#lang eopl

(define invert
    (lambda (lst)
        (map 
            (lambda (item) (list (cadr item) (car item)))
            lst))
)

(define lst '((a 1) (a 2) (1 b) (2 b)))

(display (invert lst))