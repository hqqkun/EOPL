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
  (lambda (p-name b-var body old-env)
    (let  
      ( [vec (make-vector 1)])
      (let 
        ([new-env (extend-env p-name vec old-env)])
        (vector-set! vec 0
          (proc-val (procedure b-var body new-env)))
        new-env)))
)

; apply-env : Environment * Identifier -> Expval
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" search-var))
      (extended-env-record (var val old-env)
        (if (eqv? var search-var)
          (if (expval? val)
            val
            (vector-ref val 0))
          (apply-env old-env search-var)))))
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