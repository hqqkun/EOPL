#lang eopl

(define empty-env
  (lambda ()
    '(empty-env))
)

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env))
)

(define apply-env
	(lambda (env search-var)
		(letrec
			([A (lambda (new-env)
						(cond
							((eqv? (car new-env) 'empty-env) (report-no-binding-found env search-var))
							((eqv? (car new-env) 'extend-env)
								(let 
									(	[saved-var (cadr new-env)]
										[saved-val (caddr new-env)]
										[saved-env (cadddr new-env)])
									(if (eqv? search-var saved-var) 
										saved-val
										(A saved-env))))
							(else (report-invalid-env env))))])
			(A env)))
)

(define all-bindings 
	(lambda (env)
		(if (eqv? (car env) 'empty-env)
			'()
			(let 	(	[saved-var (cadr env)]
							[saved-val (caddr env)]
							[saved-env (cadddr env)])
						(cons (list saved-var saved-val) (all-bindings saved-env)))))
)

(define report-no-binding-found
	(lambda (env search-var)
		(let ([bindings (all-bindings env)])
			(eopl:error 'apply-env "No binding for ~s, all bindings are ~s.~%" search-var bindings)))
)

(define report-invalid-env
  (lambda (env)
  	(eopl:error 'apply-env "Bad environment: ~s" env))
)

; test
(define e
    (extend-env 'd 6
        (extend-env 'y 8
            (extend-env 'x 7
                (extend-env 'y 14
                    (empty-env))))))

(display (apply-env e 'w))