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


(define apply-proc
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
        (letrec
          ( [E  (lambda (vars vals)
                  (if (null? vars)
                    saved-env
                    (extend-env (car vars) (car vals)
                      (E (cdr vars) (cdr vals)))))])
          (value-of body (E vars vals))))))
)

; value-of : Expression * Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;;letrec
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (letrec-exp (p-name b-var p-body letrec-body)
        (let 
          ( [new-env (extend-env-rec p-name b-var p-body env)])
          (value-of letrec-body new-env)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (proc-exp (vars body)
        (proc-val (procedure vars body env)))

      (call-exp (rator rands)
        (let
          ( [proc (expval->proc (value-of rator env))]
            [args (map (lambda (rand) (value-of rand env)) rands)])
          (apply-proc proc args)))
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (diff-exp (exp1 exp2)
        (num-val 
          (-  (expval->num (value-of exp1 env))
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