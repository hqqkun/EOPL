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

(define instrument (make-parameter #t))
(define cont-size 1)
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))))))  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num)
        (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (vars body)
        (apply-cont cont 
          (proc-val (procedure vars body env))))
      (letrec-exp (p-name b-vars p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-vars p-body env)
          cont))
      (zero?-exp (exp1)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp1 env
          (zero1-cont cont)))
      (let-exp (var exp1 body)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp1 env
          (let-exp-cont var body env cont)))
      (if-exp (exp1 exp2 exp3)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)))

      (diff-exp (exp1 exp2)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp1 env
          (diff1-cont exp2 env cont)))  

      (mul-exp (exp1 exp2)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp1 env
          (mul1-cont exp2 env cont)))
      
      (call-exp (rator rands)
        (set! cont-size (+ cont-size 1))
        (value-of/k rator env
          (rator-cont rands env cont)))
  )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (when (instrument)
      (eopl:printf "size of cont : ~s~%" cont-size))
    
    (set! cont-size (- cont-size 1))

    (cases continuation cont
      (end-cont () 
        (begin
          (eopl:printf
            "End of computation.~%")
          val))
      ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val
            (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
            (value-of/k exp2 saved-env saved-cont)
            (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 saved-env saved-cont)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp2
          saved-env (diff2-cont val saved-cont)))
      
      (diff2-cont (val1 saved-cont)
        (let* ( (num1 (expval->num val1))
                (num2 (expval->num val))
                (ans (- num1 num2)))
          (apply-cont saved-cont
            (num-val ans))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (mul1-cont (exp2 saved-env saved-cont)
        (set! cont-size (+ cont-size 1))
        (value-of/k exp2
          saved-env (mul2-cont val saved-cont)))
      
      (mul2-cont (val1 saved-cont)
        (let* ( (num1 (expval->num val1))
                (num2 (expval->num val))
                (ans (* num1 num2)))
          (apply-cont saved-cont
            (num-val ans))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (rator-cont (rands saved-env saved-cont)
        (let ((proc (expval->proc val)))
          (if (null? rands)
            (apply-procedure/k proc '() saved-cont)
            (begin 
              (set! cont-size (+ cont-size 1))
              (value-of/k (car rands) saved-env 
                (rands-cont proc '() (cdr rands) saved-env saved-cont))))))
      
      (rands-cont (proc vals rands saved-env saved-cont)
        (let ([new-vals (cons val vals)])
          (if (null? rands)
            (apply-procedure/k proc new-vals saved-cont)
            (begin
              (set! cont-size (+ cont-size 1))
              (value-of/k (car rands) saved-env 
                (rands-cont proc new-vals (cdr rands) saved-env saved-cont))))))
      ))
)

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          ; if you see the first line of `rands-cont`,
          ; the args is in reverse order.    
          (extend-env* vars (reverse args) saved-env)
          cont))))
)
