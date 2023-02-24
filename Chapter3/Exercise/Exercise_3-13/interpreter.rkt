#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) 
        (value-of exp1 (init-env)))))
)

; value-of : Expression * Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)  num)

      (var-exp (var) (apply-env env var))

      (diff-exp (exp1 exp2)
          (- (value-of exp1 env)
             (value-of exp2 env)))
        
      (zero?-exp (exp1)
        (if (zero? (value-of exp1 env)) 1 0))

      (if-exp (exp1 exp2 exp3)
        (if (not (zero? (value-of exp1 env)))
          (value-of exp2 env)
          (value-of exp3 env))
      )

      (let-exp (var exp1 body)
        (let*
          ( [val (value-of exp1 env)]
            [new-env (extend-env var val env)])
          (value-of body new-env)))
    ))
)

(provide value-of-program value-of)