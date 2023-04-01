#lang eopl



(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;;! These five are abstract registers.
(define exp   'uninitialized)
(define env   'uninitialized)
(define cont  'uninitialized)
(define val   'uninitialized)
(define proc1 'uninitialized) ; we've already used "proc".

; Exercise 5-31
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define num1  'uninitialized)
(define num2  'uninitialized)
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (set! exp exp1)
        (set! cont (end-cont))
        (set! env (init-env))
        (value-of/k))))
)  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! val (num-val num))
        (apply-cont))
      (var-exp (var)
        (set! val (apply-env env var))
        (apply-cont))
      (proc-exp (var body)
        (set! val (proc-val (procedure var body env)))
        (apply-cont))
      (letrec-exp (p-name b-var p-body letrec-body)
        (set! env (extend-env-rec p-name b-var p-body env))
        (set! exp letrec-body)
        (value-of/k))
      (zero?-exp (exp1)
        (set! exp exp1)
        (set! cont (zero1-cont cont))
        (value-of/k))
      (let-exp (var exp1 body)
        (set! exp exp1)
        (set! cont (let-exp-cont var body env cont))
        (value-of/k))
      (if-exp (exp1 exp2 exp3)
        (set! exp exp1)
        (set! cont (if-test-cont exp2 exp3 env cont))
        (value-of/k))
      (diff-exp (exp1 exp2)
        (set! exp exp1)
        (set! cont (diff1-cont exp2 env cont))
        (value-of/k))        
      (call-exp (rator rand)
        (set! exp rator)
        (set! cont (rator-cont rand env cont))
        (value-of/k))
  )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont () 
        (begin
          (eopl:printf
            "End of computation.~%")
          val))
      ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
      (zero1-cont (saved-cont)
        (set! val (bool-val (zero? (expval->num val))))
        (set! cont saved-cont)
        (apply-cont))
      (let-exp-cont (var body saved-env saved-cont)
        (set! exp body)
        (set! cont saved-cont)
        (set! env (extend-env var val saved-env))
        (value-of/k))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (set! cont saved-cont)
        (set! env saved-env)
        (if (expval->bool val)
            (set! exp exp2)
            (set! exp exp3))
        (value-of/k))
      (diff1-cont (exp2 saved-env saved-cont)
        (set! exp exp2)
        (set! cont (diff2-cont val saved-cont))
        (set! env saved-env)
        (value-of/k))
      (diff2-cont (val1 saved-cont)
        (set! num1 (expval->num val1))
        (set! num2 (expval->num val))
        (set! val (num-val (- num1 num2)))
        (set! cont saved-cont)
        (apply-cont))
      (rator-cont (rand saved-env saved-cont)
        (set! exp rand)
        (set! cont (rand-cont val saved-cont))
        (set! env saved-env)
        (value-of/k))
      (rand-cont (val1 saved-cont)
        (set! proc1 (expval->proc val1))
        (set! cont saved-cont)
        (apply-procedure/k))
      ))
)

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (var body saved-env)
        (set! exp body)
        (set! env (extend-env var val saved-env))
        (value-of/k))))
)
