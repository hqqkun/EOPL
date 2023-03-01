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

      (empty-list-exp () (empty-val))

      (cons-exp (exp1 exp2)
        (pair-val
          (value-of exp1 nenv)
          (value-of exp2 nenv)))

      (nameless-unpack-exp (exp1 body)
        (value-of-unpack exp1 body nenv))

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


(define value-of-unpack 
  (lambda (exp body nenv)
    (letrec
      ( [E  (lambda (_exp)
              (cases expression _exp
                (empty-list-exp () nenv)
                (cons-exp (exp1 exp2)
                  (let
                    ( [val1 (value-of exp1 nenv)]
                      [rest-env (E exp2)])
                    (extend-nameless-env val1 rest-env)))
                (else (eopl:error 'value-of "~s is not a list" exp))))])
      (value-of body (E exp))))
)
