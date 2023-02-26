#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.rkt

(require "data-structures.rkt")

(define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var))
)

; Env * Var => SchemeVa
(define apply-env
  (lambda (env search-var)
    (env search-var))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Env = Var => SchemeVal
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var)))
)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? saved-var search-var)
        saved-val
        (apply-env saved-env search-var))))
)

(define extend-env-rec
  (lambda (p-name b-var body old-env)
    (lambda (search-var)
      (if (eqv? p-name search-var)
        (proc-val 
          (procedure b-var body
            (extend-env-rec p-name b-var body old-env)))
        (apply-env old-env search-var))))
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