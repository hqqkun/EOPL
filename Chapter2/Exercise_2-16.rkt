#lang eopl

; Lc-exp  ::= Identifier
;         ::= (lambda Identifier Lc-exp)
;         ::= (Lc-exp Lc-exp)

(define var-exp
    (lambda (var) var)
)

(define lambda-exp
    (lambda (var body)
        (list 'lambda var body))
)

(define app-exp
    (lambda (rator rand) (list rator rand))
)

(define var-exp? symbol?)

(define lambda-exp?
    (lambda (exp)
        (and 
            (pair? exp)
            (eqv? (car exp) 'lambda)))
)

(define app-exp?
    (lambda (exp)
        (and 
            (pair? exp)
            (not (eqv? (car exp) 'lambda))))
)

(define var-exp->var
    (lambda (exp) exp)
)

(define lambda-exp->bound-var cadr)
(define lambda-exp->body caddr)
(define app-exp->rator car)
(define app-exp->rand cadr)