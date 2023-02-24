#lang eopl

(define identifier?
  (lambda (id)
    (and (symbol? id) (not (eqv? id 'lambda))))
)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?))
)
