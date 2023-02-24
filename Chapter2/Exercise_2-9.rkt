#lang eopl

(define has-binding?
    (lambda (env s)
        (if (null? env)
            #f
            (or 
                (eqv? s (caar env))
                (has-binding? (cdr env) s))))
)