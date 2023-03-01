#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-translation value-of)

; value-of-translation : Nameless-program -> ExpVal
(define value-of-translation
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-nameless-env)))))
)

; value-of : Nameless-exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nenv)
    (cases expression exp
      (const-exp (num)
        (num-val num))

      (nameless-var-exp (n)
        (apply-nameless-env nenv n))

      (diff-exp (exp1 exp2)
        (num-val
          (-
            (expval->num (value-of exp1 nenv))
            (expval->num (value-of exp2 nenv)))))
      
      (zero?-exp (exp1)
        (bool-val
          (zero? (expval->num (value-of exp1 nenv)))))

      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 nenv))
          (value-of exp2 nenv)
          (value-of exp3 nenv)))
      
      (nameless-let-exp (exp1 body)
        (let*
          ( [val1 (value-of exp1 nenv)]
            [new-nenv (extend-nameless-env val1 nenv)])
          (value-of body new-nenv)))

      (call-exp (rator rand)
        (let
          ( [proc (expval->proc (value-of rator nenv))]
            [arg (value-of rand nenv)])
          (apply-proc proc arg)))

      (nameless-proc-exp (body)
        (proc-val 
          (procedure body nenv)))

      (cond-exp (exp-lst1 exp-lst2)
        (value-of-cond exp-lst1 exp-lst2 nenv))
      
      (else
         (eopl:error 'value-of 
	    "Illegal expression in translated code: ~s" exp))))
)

(define apply-proc
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (body saved-env)
        (value-of
          body
          (extend-nameless-env arg saved-env)))))
)

; value-of-cond : ListOf(exp) * ListOf(exp) * nenv -> ExpVal
(define value-of-cond
  (lambda (exp-lst1 exp-lst2 nenv)
    (letrec
      ( [V  (lambda (exp-l1 exp-l2)
              (cond
                ((null? exp-l1) (report-cond-false (cond-exp exp-lst1 exp-lst2)))
                ((eqv? #t (expval->bool (value-of (car exp-l1) nenv)))
                  (value-of (car exp-l2) nenv))
                (else (V (cdr exp-l1) (cdr exp-l2)))))])
      (V exp-lst1 exp-lst2)))
)

(define report-cond-false
  (lambda (exp)
      (eopl:error 'value-of "at least one condition must be true! exp : ~s" exp))
)