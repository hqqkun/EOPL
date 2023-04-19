#lang eopl

;; cps interpreter for the LETREC language, using the data structure
;; representation of continuations (Figure 5.3).

;; exercise: rewrite this using the procedural representation of
;; continuations (Figure 5.2).

;; exercise: rewrite this using a trampoline (page 159).

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define end-cont
  (lambda (val)
    (begin
      (eopl:printf
        "End of computation.~%")
      val))
)


;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) end-cont)))))  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (cont (num-val num)))
      (var-exp (var) (cont (apply-env env var)))
      (proc-exp (var body)
        (cont 
          (proc-val (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont))
      (zero?-exp (exp1)
        (value-of/k exp1 env
          (lambda (val1)
            (cont (bool-val (zero? (expval->num val1)))))))
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (lambda (val1)
            (value-of/k body (extend-env var val1 env)
              cont))))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (lambda (val1)
            (if (expval->bool val1)
              (value-of/k exp2 env cont)
              (value-of/k exp3 env cont)))))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (lambda (val1)
            (value-of/k exp2 env
              (lambda (val2)
                (let
                  ( [num1 (expval->num val1)]
                    [num2 (expval->num val2)])
                  (cont (num-val (- num1 num2)))))))))        
      (call-exp (rator rand) 
        (value-of/k rator env
          (lambda (val1)
            (value-of/k rand env
              (lambda (arg)
                (cases proc (expval->proc val1)
                  (procedure (var body saved-env)
                    (value-of/k body
                      (extend-env var arg saved-env) cont))))))))
  ))
)
