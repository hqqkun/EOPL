#lang eopl

; 
(define g
    (lambda (lst lop)
        (if (null? lop)
            (list lst)
            (cons lst
                (map 
                    (lambda (p) (list (+ 1 (car p)) (cadr p)))
                    lop)))
    )
)

; List -> ListOf(List(Int, SchemeVal))
; lop means list of pairs
(define number-elements
    (lambda (lst)
        (if (null? lst)
            '()
            (g 
                (list 0 (car lst))
                (number-elements (cdr lst)))))
)

; test
(define lst '(a b c d e))
(display (number-elements lst))
