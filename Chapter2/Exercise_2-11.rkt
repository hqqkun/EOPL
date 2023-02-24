#lang eopl

(define empty-env
    (lambda () '())
)

(define extend-env*
    (lambda (vars vals env)
        (cons (cons vars vals) env))
)

(define extend-env
    (lambda (var val env)
        (extend-env* (list var) (list val) env)
    )
)

; Env * Symbol -> Int
(define apply-env
	(lambda (env search-var)
		(letrec
			; A return a pair => (find?, find-value)
			([A (lambda (vars vals)
						(cond
							((null? vars) (cons #f #f))
							((eqv? (car vars) search-var) (cons #t (car vals)))
							(else (A (cdr vars) (cdr vals)))
						))])
				
			(if (null? env)
				(report-no-binding-found search-var)
				(let* 
					(	[first-res (A (caar env) (cdar env))]
						[find? (car first-res)]
						[find-value (cdr first-res)])
					(if find?
						find-value
						(apply-env (cdr env) search-var))))))
)

(define report-no-binding-found
	(lambda (search-var)
		(eopl:error 'apply-env "No binding for ~s" search-var))
)

; test
(define e 
	(extend-env* '(a b c) '(11 12 13)
		(extend-env* '(x z) '(66 77)
			(extend-env* '(x y) '(88 99) (empty-env))))
)

(display (apply-env e 'z))