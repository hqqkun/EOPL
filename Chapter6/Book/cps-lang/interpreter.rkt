#lang eopl

(require "drscheme-init.rkt")

(require "cps-out-lang.rkt")
(require "data-structures.rkt")       ; this includes environments

(provide value-of-program value-of/k)


(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
      (cps-a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont)))))
)

(define value-of/k 
  (lambda (exp env cont)
    1)
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
          ( [nums (map (lambda (exp) (value-of-simple-exp exp env)) exps)])
          (num-val (reduce + nums))))

      ))
)



(define (reduce func list)
  (assert (not (null? list)))
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list))))
)