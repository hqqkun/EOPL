#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.rkt

(require "data-structures.rkt")

(define empty-env
  (lambda () (empty-env-record))
)

(define empty-env?
  (lambda (env)
    (cases environment env
      (empty-env-record () #t)
      (else #f)))
)

(define extend-env
  (lambda (var val old-env)
    (extended-env-record var val old-env))
)

(define extend-env-rec
  (lambda (p-name b-vars body old-env)
    (extended-env-rec-record
      p-name b-vars body old-env))
)

; apply-env : Environment * Identifier -> Expval
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" search-var))
      (extended-env-record (var val old-env)
        (if (eqv? var search-var) 
          val
          (apply-env old-env search-var)))
      (extended-env-rec-record (p-names b-vars bodys old-env)
        (letrec
          ; Exercise 3-23 helper function
          ; since now p-names b-vars and bodys are all lists, and
          ; each member of b-vars is also a list.
          ( [A (lambda (p-names b-vars bodys)
                (cond
                  ((null? p-names) (apply-env old-env search-var))
                  ((eqv? search-var (car p-names))
                    (proc-val (procedure (car b-vars) (car bodys) env)))
                  (else (A (cdr p-names) (cdr b-vars) (cdr bodys)))))])
           (A p-names b-vars bodys)))))
)


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  
(define init-env 
  (lambda ()
    (extend-env 
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
        (extend-env
          'x (num-val 10)
          (empty-env)))))
)

(provide (all-defined-out)) 