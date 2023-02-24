#lang eopl

; big digit 
(define base 10)
(define max-rep 9)

(define zero
    (lambda () '())
)

(define is-zero? null?)

; bigits -> bigits
(define successor
    (lambda (n)
        (cond
            ((is-zero? n) '(1))
            ((eqv? (car n) max-rep) (cons 0 (successor (cdr n))))
            (else (cons (+ 1 (car n)) (cdr n)))))
)

; bigits -> bigits
(define predecessor 
    (lambda (n)
        (cond
            ((is-zero? n) n)
            ((equal? n '(1)) '())
            ((eqv? (car n) 0) (cons max-rep (predecessor (cdr n))))
            (else (cons (- (car n) 1) (cdr n)))))
)

; bigits * bigits -> bigits
(define add 
    (lambda (a b) 
        (if (is-zero? b)
            a
            (successor (add a (predecessor b)))))
)

; bigits * bigits -> bigits
(define mul 
    (lambda (a b)
        (if (is-zero? b)
            (zero)
            (add (mul a (predecessor b)) a)))
)

; bigits -> bigits
(define factorial
    (lambda (n)
        (cond
            ((is-zero? n) '(1))
            ((equal? n '(1)) '(1))
            (else (mul n (factorial (predecessor n))))))
)

; test
(define num '(0 1))
(display (factorial num))