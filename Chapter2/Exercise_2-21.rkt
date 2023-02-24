#lang eopl

(define-datatype env env?
  (empty-env)
  (extended-env
   (var symbol?)
   (val always?)
   (rest-env env?))
  )

(define extend-env
  (lambda (var val e)
    (cases env e
      (else (extended-env var val e))))
  )

(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (extended-env (var val rest-env)
        (if (eqv? var search-var)
          val
          (apply-env rest-env search-var)))))
)

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s~%" search-var))
)

; test
(define e 
  (extend-env 'x 12
    (extend-env 'y 45
      (extend-env 'z 56
        (extend-env 'y 7
          (empty-env)))))
)

(display (apply-env e 'w))