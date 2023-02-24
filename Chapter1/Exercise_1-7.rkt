#lang eopl

(define nth-element
    (lambda (lst n)
        (letrec 
            ([nth-element-helper 
                (lambda (_l _n)
                    (cond
                        ((null? _l) (report-list-too-short lst n))
                        ((zero? _n) (car _l))
                        (else (nth-element-helper (cdr _l) (- _n 1)))))])
            (nth-element-helper lst n)))
)

(define report-list-too-short
    (lambda (lst n)
        (eopl:error 'nth-element
            "~s does not have ~s elements.~%" lst (+ n 1)))
)

; test
(define lst '(1 2 3))
(display (nth-element lst 2))
(nth-element lst 45)