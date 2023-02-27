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

; Exercise 3-36
(define extend-env-rec
  (lambda (p-names b-vars bodys old-env)
    (let* 
      ( [len (length p-names)]
        [vec (make-vector len)])
      (let ([new-env (extend-env p-names vec old-env)])
        (letrec 
          ( [S  (lambda (b-vars bodys index)
                  (if (null? b-vars)
                    0
                    (begin
                      (vector-set! vec index 
                        (proc-val (procedure (car b-vars) (car bodys) new-env)))
                      (S (cdr b-vars) (cdr bodys) (+ index 1)))))])
          (S b-vars bodys 0)
          new-env))))
)

; find rec proc in this vector based environment
(define find-rec-proc
  (lambda (p-names vec old-env search-var)
    (letrec
      ( [F  (lambda (p-names index)
              (cond
                ((null? p-names) (apply-env old-env search-var))
                ((eqv? (car p-names) search-var) (vector-ref vec index))
                (else (F (cdr p-names) (+ index 1)))))])
      (F p-names 0)))
)

; apply-env : Environment * Identifier -> Expval
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" search-var))
      
      (extended-env-record (var val old-env)
        (if (list? var)
          (find-rec-proc var val old-env search-var)
          (if (eqv? var search-var) val (apply-env old-env search-var))))))
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