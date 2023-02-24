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
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (minus-exp (exp1) 
        (num-val 
          (- (expval->num (value-of exp1 env)))))
        
      (diff-exp (exp1 exp2)
        (num-val 
          (-  (expval->num (value-of exp1 env))
              (expval->num (value-of exp2 env)))))
      (add-exp (exp1 exp2)
        (num-val
          (+ (expval->num (value-of exp1 env))
             (expval->num (value-of exp2 env)))))

      (mul-exp (exp1 exp2)
        (num-val 
          (* (expval->num (value-of exp1 env))
             (expval->num (value-of exp2 env)))))

      (div-exp (exp1 exp2)
        (num-val
          (quotient (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env)))))
        
      (zero?-exp (exp1)
        (let* ( [val (value-of exp1 env)]
                [num (expval->num val)])
          (if (zero? num) 
            (bool-val #t)
            (bool-val #f))))

      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))

      (let-exp (var exp1 body)
        (let*
          ( [val (value-of exp1 env)]
            [new-env (extend-env var val env)])
          (value-of body new-env)))
    ))
)

(provide value-of-program value-of)