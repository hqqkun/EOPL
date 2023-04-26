#lang eopl

(require "drscheme-init.rkt")
(require "cps-in-lang.rkt")



(define tail-form?
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
        (tail-form-exp? exp))))
)

(define tail-form-exp?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)

      (diff-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2)))
      
      (zero?-exp (exp1)
        (simple-exp? exp1))

      (proc-exp (vars body)
        (tail-form-exp? body))
      
      (if-exp (exp1 exp2 exp3)
        (and 
          (simple-exp? exp1)
          (tail-form-exp? exp2)
          (tail-form-exp? exp3)))
      (let-exp (var exp1 body)
        (and (simple-exp? exp1)
          (tail-form-exp? body)))
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (if (all-sat tail-form-exp? p-bodies)
          (tail-form-exp? letrec-body)
          #f))
      (sum-exp (exps)
        (all-sat simple-exp? exps))
      (call-exp (rator rands)
        (and (simple-exp? rator)
          (all-sat simple-exp? rands)))
    ))
)


(define simple-exp?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)

      (diff-exp (exp1 exp2)
        (and (simple-exp? exp1) (simple-exp? exp2)))
      
      (zero?-exp (exp1)
        (simple-exp? exp1))

      (proc-exp (vars body)
        (tail-form-exp? body))
      (else #f)
    ))
)

(define all-sat
  (lambda (pred? lst)
    (letrec
      ( [A  (lambda (lst)
              (cond
                ((null? lst) #t)
                ((pred? (car lst)) (A (cdr lst)))
                (else #f)))])
      (A lst)))
)



(define str "let p = proc(x) 1 in -((p 1), 2)")
(define run
  (lambda (str)
    (tail-form? (scan&parse str)))
)

(display (run str))