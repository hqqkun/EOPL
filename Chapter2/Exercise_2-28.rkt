#lang eopl

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
    (var-exp (var identifier?))
    (lambda-exp
        (bound-var identifier?)
        (body lc-exp?)
    )
    (app-exp
        (rator lc-exp?)
        (rand lc-exp?)
    )
)

; lc-exp -> string
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
		(var-exp (var) (symbol->string var))
		(lambda-exp (bound-var body)
			(string-append 
				"(lambda ("
				(symbol->string bound-var)
				") "
				(unparse-lc-exp body)
				")"))
		(app-exp (rator rand)
			(string-append
				"("
				(unparse-lc-exp rator)
				" "
				(unparse-lc-exp rand)
				")"))))
)

; test
(define exp (lambda-exp 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))))
(display (unparse-lc-exp exp))