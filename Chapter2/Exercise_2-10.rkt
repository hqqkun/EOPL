#lang eopl

(define extend-env*
    (lambda (lovar loval env)
        (if (null? lovar)
            env
            (cons 
                (cons (car lovar) (car loval))
                (extend-env* (cdr lovar) (cdr loval) env))))
)
