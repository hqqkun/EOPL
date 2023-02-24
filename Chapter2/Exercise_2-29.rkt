#lang eopl

(define identifier?
    (lambda (x)
        (and (symbol? x) (not (eqv? x 'lambda))))
)

(define-datatype lc-exp lc-exp?
    (var-exp (var identifier?))
    (lambda-exp
        (bound-vars (list-of identifier?))
        (body lc-exp?)
    )
    (app-exp
        (rator lc-exp?)
        (rands (list-of lc-exp?))
    )
)

(define report-invalid-concrete-syntax
    (lambda (cexp)
        (eopl:error "invalid concrete syntax ~s" cexp))
)

; concrete expression
; (lambda ({Identifier}∗) Lc-exp)
(define cexp->vars cadr)
(define cexp->body caddr)

; cexp -> lc-exp
; cexp means concrete expression
(define parse-expression
    (lambda (cexp)
        (cond
            ((identifier? cexp) (var-exp cexp))
            ((pair? cexp) 
                (if (eqv? (car cexp) 'lambda)
                    (lambda-exp 
                        (cexp->vars cexp)
                        (parse-expression (cexp->body cexp)))
                    (app-exp
                        (parse-expression (car cexp))
                        (map parse-expression (cdr cexp)))))
            (else (report-invalid-concrete-syntax cexp))))
)

; test
(define cexp '(lambda (x y z) (x y z)))
(display (parse-expression cexp))