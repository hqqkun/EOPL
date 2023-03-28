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
        (value-of/k exp1 (init-env) (end-cont))))))  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont 
          (proc-val (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont))
      
      (zero?-exp (exp1)
        (value-of/k exp1 env
          (cons (zero1-frame) cont)))
      
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (cons (let-exp-frame var body env) cont)))
      
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (cons (if-test-frame exp2 exp3 env) cont)))
      
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (cons (diff1-frame exp2 env) cont)))
            
      (call-exp (rator rand) 
        (value-of/k rator env
          (cons (rator-frame rand env) cont)))
  )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (if (null? cont)
      (begin
        (eopl:printf "End of computation.~%")
        val)
      (let
        ( [frame1 (car cont)]
          [saved-cont (cdr cont)])
        (cases frame frame1
          (zero1-frame ()
            (apply-cont saved-cont
              (bool-val (zero? (expval->num val)))))

          (let-exp-frame (var body saved-env)
            (value-of/k body
              (extend-env var val saved-env) 
              saved-cont))
          
          (if-test-frame (exp2 exp3 saved-env)
            (if (expval->bool val)
              (value-of/k exp2 saved-env saved-cont)
              (value-of/k exp3 saved-env saved-cont)))
          
          (diff1-frame (exp2 saved-env)
            (value-of/k exp2 saved-env
              (cons (diff2-frame val) saved-cont)))
          
          (diff2-frame (val1)
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val)])
              (apply-cont saved-cont
                (num-val (- num1 num2)))))
          
          (rator-frame (rand saved-env)
            (value-of/k rand saved-env
              (cons (rand-frame val) saved-cont)))

          (rand-frame (val1)
            (let ([proc (expval->proc val1)])
              (apply-procedure/k proc val saved-cont)))
        ))))
)

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body
          (extend-env var arg saved-env)
          cont))))
)
