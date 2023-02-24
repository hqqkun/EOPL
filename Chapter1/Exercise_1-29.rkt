#lang eopl

; insertion sort
; loi -> loi
(define sort 
    (lambda (loi)
        (if (null? loi) 
            '()
            ; assume cdrOf(loi) is sorted
            (S (car loi) (sort (cdr loi)))))
)

; helper function
(define S
    (lambda (num los)
        (letrec 
            ([H (lambda (loi)
                    (cond 
                        ((null? loi) (list num))
                        ((< num (car loi)) (cons num loi))
                        (else (cons (car loi) (H (cdr loi))))))])
            (H los)
        ))
)

; test
(define loi '(8 2 5 2 3))
(display (sort loi))