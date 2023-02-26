#lang eopl

; env is a list of two procedures.
(define empty-env
  (lambda ()
    (list
      (lambda (search-var) (report-no-binding-found search-var))
      (lambda () #t)))
)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
      (lambda (search-var)
        (if (eqv? saved-var search-var)
          saved-val
          (apply-env saved-env search-var)))
      (lambda () #f)))
)

(define apply-env
  (lambda (env search-var)
    ((car env) search-var))
)

(define empty-env?
  (lambda (env)
    ((cadr env)))
)

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var))
)

; test
(define e
  (extend-env 'd 6
    (extend-env 'y 8
      (extend-env 'x 7
        (extend-env 'y 14
          (empty-env))))))

(display (empty-env? e))