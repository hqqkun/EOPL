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

; value-of-let-exp :
;   ListOf(vars) * ListOf(Expression) * Expression * Environment
;       ->
;   ExpVal
(define value-of-let-exp
  (lambda (vars val-exps body env)
    (letrec
      ([E (lambda (vars val-exps)
            (if (null? vars)
              env
              (let*
                ( [rest-env (E (cdr vars) (cdr val-exps))]
                  [var (car vars)]
                  [exp1 (car val-exps)]
                  [val1 (value-of exp1 env)])
                (extend-env var val1 rest-env))))])
      (let
        ( [new-env (E vars val-exps)])
        (value-of body new-env))))
)

(define apply-proc
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var body)
        (value-of body (extend-env var val env)))))
)

; value-of : Expression * Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;;Exercise
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (proc-exp (var body)
        (proc-val (procedure var body)))

      (call-exp (rator rand)
        (let
          ( [proc (expval->proc (value-of rator env))]
            [arg (value-of rand env)])
          (apply-proc proc arg env)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
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

      (let-exp (vars val-exps body)
        (value-of-let-exp vars val-exps body env))
    ))
)

(provide value-of-program value-of)