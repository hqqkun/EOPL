#lang eopl

(define down
    (lambda (lst)
        (map 
            (lambda (item) (list item))
            lst))
)

; test
(define lst '(1 2 3))
(display (down lst))