#lang eopl

(define product 
    (lambda (sos1 sos2)
        (letrec 
            ([P 
                (lambda (sos)
                    (if (null? sos) 
                        '()
                            (append
                                (map (lambda (item) (list (car sos) item)) sos2)
                                (P (cdr sos)))))])
            (P sos1))
    )
)

; test
(display  (product '(a b c) '(x y)))