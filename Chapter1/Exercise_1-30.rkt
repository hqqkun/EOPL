#lang eopl

(define sort/predicate
    (lambda (pred loi)
        (letrec 
            ([S (lambda (num loi)
                    (cond
                        ((null? loi) (list num))
                        ((pred num (car loi)) (cons num loi))
                        (else (cons (car loi) (S num (cdr loi))))))])
            (if (null? loi)
                '()
                (S (car loi) (sort/predicate pred (cdr loi))))))
)

; test
(define loi '(8 2 5 2 3))
(display (sort/predicate < loi))
(newline)
(display (sort/predicate > loi))