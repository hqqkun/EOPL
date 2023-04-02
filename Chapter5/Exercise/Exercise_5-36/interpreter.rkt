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

;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont) (end-cont))))))  

;; value-of/k : Exp * Env * Cont * Cont -> FinalAnswer
;;! try-cont1 tells which exception cont should we go.
(define value-of/k
  (lambda (exp env cont try-cont1)
    (cases expression exp
      ;; new staff
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (try-exp (exp1 var handler-exp)
        (let
          ([new-try-cont (try-cont var handler-exp env cont try-cont1)])
          (value-of/k exp1 env new-try-cont new-try-cont)))
      (raise-exp (exp1)
        (value-of/k exp1 env
          (raise1-cont try-cont1) try-cont1))
      (unop-exp (unop exp1)
        (value-of/k exp1 env
          (unop-arg-cont unop cont try-cont1) try-cont1))
      (const-list-exp (nums)
        (apply-cont cont
          (list-val (map num-val nums))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont 
          (proc-val (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont try-cont1))
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont try-cont1) try-cont1))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont try-cont1) try-cont1))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (diff1-cont exp2 env cont try-cont1) try-cont1))        
      (call-exp (rator rand) 
        (value-of/k rator env
          (rator-cont rand env cont try-cont1) try-cont1))
  )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      ;; new staff
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (try-cont (var handler-exp saved-env saved-cont try-cont1)
        (apply-cont saved-cont val))
      (raise1-cont (try-cont1)
        (apply-handler val try-cont1))
      (unop-arg-cont (unop saved-cont try-cont1)
        (apply-cont saved-cont
          (apply-unop unop val)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (end-cont () 
        (begin
          (eopl:printf
            "End of computation.~%")
          val))
      (let-exp-cont (var body saved-env saved-cont try-cont1)
        (value-of/k body
          (extend-env var val saved-env) saved-cont try-cont1))
      (if-test-cont (exp2 exp3 saved-env saved-cont try-cont1)
        (if (expval->bool val)
            (value-of/k exp2 saved-env saved-cont try-cont1)
            (value-of/k exp3 saved-env saved-cont try-cont1)))
      (diff1-cont (exp2 saved-env saved-cont try-cont1)
        (value-of/k exp2
          saved-env (diff2-cont val saved-cont try-cont1) try-cont1))
      (diff2-cont (val1 saved-cont try-cont1)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
          (apply-cont saved-cont
            (num-val (- num1 num2)))))
      (rator-cont (rand saved-env saved-cont try-cont1)
        (value-of/k rand saved-env
          (rand-cont val saved-cont try-cont1) try-cont1))
      (rand-cont (val1 saved-cont try-cont1)
        (let ((proc (expval->proc val1)))
          (apply-procedure/k proc val saved-cont try-cont1)))
      ))
)

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont try-cont1)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body
          (extend-env var arg saved-env)
          cont try-cont1))))
)


(define apply-handler 
  (lambda (val try-cont1)
    (cases continuation try-cont1
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (try-cont (var handler-exp saved-env saved-cont try-cont1)
        (value-of/k handler-exp
          (extend-env var val saved-env)
          saved-cont try-cont1))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (end-cont () 
        (eopl:error 
          'apply-handler "uncaught exception!"))
      (else 
        (eopl:error
          'apply-handler "something must go wrong!"))
      ))
)

; apply-unop : UnOp * ExpVal -> ExpVal
(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      (null?-unop ()
        (bool-val
          (null? (expval->list val))))
      (car-unop ()
        (car (expval->list val)))
      (cdr-unop ()
        (list-val
          (cdr (expval->list val))))
      (zero?-unop ()
        (bool-val
          (zero? (expval->num val))))
    ))
)
