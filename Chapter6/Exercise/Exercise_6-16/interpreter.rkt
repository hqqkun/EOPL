#lang eopl

(require "drscheme-init.rkt")

(require "cps-out-lang.rkt")
(require "data-structures.rkt")       ; this includes environments

(provide value-of-program value-of/k)



;; register
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)
(define args 'uninitialized)


(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
      (cps-a-program (exp1)
        (set! exp exp1)
        (set! env (init-env))
        (set! cont (end-cont))
        (value-of/k))))
)

(define value-of/k 
  (lambda ()
    (cases tfexp exp
      (simple-exp->exp (simple)
        (set! val (value-of-simple-exp simple env))
        (apply-cont))
      (cps-let-exp (var rhs body)
        (set! val (value-of-simple-exp rhs env))
        (set! env (extend-env* (list var) (list val) env))
        (set! exp body)
        (value-of/k))
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (set! exp letrec-body)
        (set! env (extend-env-rec** p-names b-varss p-bodies env))
        (value-of/k))
      (cps-if-exp (simple1 body1 body2)
        (set! val (value-of-simple-exp simple1 env))
        (if (expval->bool val)
          (set! exp body1)
          (set! exp body2))
        (value-of/k))
      (cps-call-exp (rator rands)
        (set! proc1 (expval->proc (value-of-simple-exp rator env)))
        (set! args (map (lambda (simple) (value-of-simple-exp simple env)) rands))
        (apply-procedure/k))

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
          (num-val (reduce + nums))))
      ))
)



(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (vars body saved-env)
        (set! exp body)
        (set! env (extend-env* vars args saved-env))
        (value-of/k))))
)

(define reduce
  (lambda (func list)
    (if (null? (cdr list))
        (car list)
        (func (car list) (reduce func (cdr list)))))
)


(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf
            "End of computation.~%")
          val))))
)


(define str "let p = proc(x y) +(x, y, 15) in (p 1 2)")
(define run
  (lambda (str)
    (display 
      (value-of-program (cps-out-scan&parse str))))
)

(run str)