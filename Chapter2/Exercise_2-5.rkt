#lang eopl

(define empty-env
    (lambda () '())
)

(define extend-env
    (lambda (var val env)
        (cons (cons var val) env))
)

(define apply-env
    (lambda (env search-var)
        (if (eqv? search-var (car (car env)))
            (cdr (car env))
            (apply-env (cdr env) search-var)))
)

; test
(define e
    (extend-env 'd 6
        (extend-env 'y 8
            (extend-env 'x 7
                (extend-env 'y 14
                    (empty-env))))))

(display (apply-env e 'x))