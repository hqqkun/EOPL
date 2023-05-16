#lang eopl

(require "drscheme-init.rkt")
(require "store.rkt")
(require "cps-out-lang.rkt")
(require "data-structures.rkt")       ; this includes environments

(provide value-of-program value-of/k)


(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases cps-out-program pgm
      (cps-a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont)))))
)

(define value-of/k 
  (lambda (exp env cont)
    (cases tfexp exp
      (simple-exp->exp (simple)
        (apply-cont cont
          (value-of-simple-exp simple env)))
      (cps-let-exp (var rhs body)
        (let ([val (value-of-simple-exp rhs env)])
          (value-of/k body
            (extend-env* (list var) (list val) env)
            cont)))
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec** p-names b-varss p-bodies env)
          cont))
      (cps-if-exp (simple1 body1 body2)
        (if (expval->bool (value-of-simple-exp simple1 env))
          (value-of/k body1 env cont)
          (value-of/k body2 env cont)))
      (cps-call-exp (rator rands)
        (let
          ( [rator-proc (expval->proc (value-of-simple-exp rator env))]
            [rand-vals (map (lambda (simple) (value-of-simple-exp simple env)) rands)])
          (apply-procedure/k rator-proc rand-vals cont)))
      (cps-printk-exp (simple body)
        (begin
          (eopl:printf "~s~%" (value-of-simple-exp simple env))
          (value-of/k body env cont)))

      (cps-newrefk-exp (simple1 simple2)
        (let
          ( [val1 (value-of-simple-exp simple1 env)]
            [val2 (value-of-simple-exp simple2 env)])
          (let ([newval (ref-val (newref val1))])
            (apply-procedure/k
              (expval->proc val2)
              (list newval) cont))))
      
      (cps-derefk-exp (simple1 simple2)
        (let
          ( [val1 (value-of-simple-exp simple1 env)]
            [val2 (value-of-simple-exp simple2 env)])
          (let ([val (deref (expval->ref val1))])
            (apply-procedure/k
              (expval->proc val2)
              (list val) cont))))

      (cps-setrefk-exp (simple1 simple2 body)
        (let
          ( [val1 (value-of-simple-exp simple1 env)]
            [val2 (value-of-simple-exp simple2 env)])
          (begin
            (setref! (expval->ref val1) val2)
            (value-of/k body env cont))))
    ))
)


(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
      (cps-const-exp (num)
        (num-val num))
      (cps-var-exp (var)
        (apply-env env var))
      (cps-proc-exp (vars body)
        (proc-val (procedure vars body env)))
      (cps-diff-exp (exp1 exp2)
        (let
          ( [num1 (expval->num (value-of-simple-exp exp1 env))]
            [num2 (expval->num (value-of-simple-exp exp2 env))])
          (num-val (- num1 num2))))
      (cps-zero?-exp (exp1)
        (let ([num1 (expval->num (value-of-simple-exp exp1 env))])
          (bool-val (zero? num1))))
      (cps-sum-exp (exps)
        (let
          ( [nums (map (lambda (exp) (expval->num (value-of-simple-exp exp env))) exps)])
          (num-val (add-reduce nums))))
      ))
)



(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (extend-env* vars args saved-env)
          cont))))
)

(define add-reduce
  (lambda (nums)
    (let loop ([nums nums] [sum 0])
      (if (null? nums)
        sum
        (loop (cdr nums) (+ sum (car nums))))))
)

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf
            "End of computation.~%")
          val))))
)
