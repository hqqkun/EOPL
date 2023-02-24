#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(require scheme/base)

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) 
        (value-of exp1 (init-env)))))
)


(define apply-proc
  (lambda (proc1 val)
    (cases proc proc1
      (trace-procedure (var body saved-env)
        (begin
          (printf "enter proc : ~s = ~s)~%" var val)
          (let
            ( [res (value-of body (extend-env var val saved-env))])
            (begin
              (printf "leave proc : res = ~s~%" res)
              res))))

      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env)))))
)

; value-of : Expression * Environment -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;;traceproc
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (traceproc-exp (var body)
        (proc-val (trace-procedure var body env)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (proc-exp (var body)
        (proc-val (procedure var body env)))

      (call-exp (rator rand)
        (let
          ( [proc (expval->proc (value-of rator env))]
            [arg (value-of rand env)])
          (apply-proc proc arg)))
      
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