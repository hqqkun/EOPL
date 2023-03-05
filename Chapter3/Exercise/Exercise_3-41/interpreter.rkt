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

      (nameless-var-exp (lex-depth index)
        (apply-nameless-env nenv lex-depth index))

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
      
      (nameless-let-exp (exp-lst body)
        (let*
          ( [val-lst (map (lambda (exp) (value-of exp nenv)) exp-lst)]
            [new-nenv (extend-nameless-env* val-lst nenv)])
          (value-of body new-nenv)))

      (call-exp (rator rands)
        (let
          ( [proc (expval->proc (value-of rator nenv))]
            [args (map (lambda (rand) (value-of rand nenv)) rands)])
          (apply-proc proc args)))

      (nameless-proc-exp (body)
        (proc-val 
          (procedure body nenv)))

      (else
         (eopl:error 'value-of 
	    "Illegal expression in translated code: ~s" exp))))
)

; apply-proc : Proc * ListOf(ExpVal) -> ExpVal
(define apply-proc
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (body saved-env)
        (value-of
          body
          (extend-nameless-env* args saved-env)))))
)
