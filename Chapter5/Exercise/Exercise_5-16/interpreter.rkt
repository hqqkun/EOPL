#lang eopl

(require "drscheme-init.rkt")
(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")

(provide value-of-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmt1)
        (initialize-store!)
        (result-of/k stmt1 (init-env) (end-cmd-cont))))
  ))


(define result-of/k
  (lambda (stmt env cmd-cont)
    (cases statement stmt

      (assign-stmt (var exp)
        (setref! 
          (apply-env env var)
          (value-of/k exp env (end-cont)))
        (apply-command-cont cmd-cont))  
      
      (print-stmt (exp1)
        (cases expval (value-of/k exp1 env (end-cont))
          (num-val (num) 
            (eopl:printf "~s\n" num))
          (bool-val (bool)
            (eopl:printf "~s\n" bool))
          (ref-val (ref)
            (eopl:printf "loc : ~s\n" ref))
          (proc-val (proc)
            (eopl:printf "proc : ~s\n" proc)))
        (apply-command-cont cmd-cont))

        (seqs-stmt (stmts)
          (if (null? stmts)
            (apply-command-cont cmd-cont)
            (result-of/k (car stmts) env
              (seqs-cmd-cont (cdr stmts) env cmd-cont))))
        
        (if-stmt (exp1 stmt1 stmt2)
          (if (expval->bool (value-of/k exp1 env (end-cont)))
            (result-of/k stmt1 env cmd-cont)
            (result-of/k stmt2 env cmd-cont)))

        (block-stmt (vars stmt1)
          (let 
            ( [new-env (extend-env-uninit* vars env)])
            (result-of/k stmt1 new-env cmd-cont)))
        
        (while-stmt (exp1 stmt1)
          (let
            ( [val (expval->bool (value-of/k exp1 env (end-cont)))])
            (if val
              (result-of/k stmt1 env
                (while-cmd-cont
                  stmt env cmd-cont))
              (apply-command-cont cmd-cont))))
  ))
)

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      (proc-exp (var body)
        (apply-cont cont 
          (proc-val (procedure var body env))))
      (letrec-exp (p-names b-vars p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec* p-names b-vars p-body env)
          cont))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (diff1-cont exp2 env cont)))      
      (zero?-exp (exp1)
        (value-of/k exp1 env
          (zero1-cont cont)))         
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)))
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont)))
      (call-exp (rator rands) 
        (value-of/k rator env
          (rator-cont rands env cont)))
      (begin-exp (exp1 exps)
        (value-of/k exp1 env
          (begin-cont exps env cont)))
      (assign-exp (var exp1)
        (value-of/k exp1 env
          (set-rhs-cont var env cont)))
      (add-exp (exp1 exp2)
        (value-of/k exp1 env 
          (add1-cont exp2 env cont)))
      (mul-exp (exp1 exp2)
        (value-of/k exp1 env 
          (mul1-cont exp2 env cont)))
      (not-exp (exp1)
        (value-of/k exp1 env
          (not-cont cont)))
      ))
)

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        val)
      ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val
            (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var (newref val) saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
            (value-of/k exp2 saved-env saved-cont)
            (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2
          saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
          (apply-cont saved-cont
            (num-val (- num1 num2)))))
      (add1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2
          saved-env (add2-cont val saved-cont)))
      (add2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
          (apply-cont saved-cont
            (num-val (+ num1 num2)))))    
      (mul1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2
          saved-env (mul2-cont val saved-cont)))
      (mul2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
          (apply-cont saved-cont
            (num-val (* num1 num2)))))
      (not-cont (saved-cont)
        (let ([bool (expval->bool val)])
          (apply-cont saved-cont
            (bool-val (not bool)))))
      (rator-cont (rands saved-env saved-cont)
        (let ((proc (expval->proc val)))
          (if (null? rands)
            (apply-procedure/k proc '() saved-cont)
            (value-of/k (car rands) saved-env 
              (rands-cont proc '() (cdr rands) saved-env saved-cont)))))
      
      (rands-cont (proc vals rands saved-env saved-cont)
        (let ([new-vals (cons val vals)])
          (if (null? rands)
            (apply-procedure/k proc new-vals saved-cont)
            (value-of/k (car rands) saved-env 
              (rands-cont proc new-vals (cdr rands) saved-env saved-cont)))))
      (set-rhs-cont (var saved-env saved-cont)
        (begin
          (setref! (apply-env saved-env var) val)
          (apply-cont saved-cont (num-val 27))))
      (begin-cont (exps saved-env saved-cont)
        (if (null? exps)
          (apply-cont saved-cont val)
          (value-of/k (car exps) saved-env
            (begin-cont (cdr exps) saved-env saved-cont))))
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


; Exercise 5-16
; command continuation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-command-cont
  (lambda (cont)
    (cases cmd-continuation cont
      (end-cmd-cont ()
        (eopl:printf "End of Pragram.~%"))
    
      (seqs-cmd-cont (stmts saved-env saved-cont)
        (if (null? stmts)
          (apply-command-cont saved-cont)
          (result-of/k (car stmts) saved-env
            (seqs-cmd-cont (cdr stmts) saved-env saved-cont))))
      (while-cmd-cont (stmt saved-env saved-cont)
        (result-of/k stmt saved-env saved-cont))
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
