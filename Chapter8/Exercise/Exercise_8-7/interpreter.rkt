#lang eopl

(require "drscheme-init.rkt")

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of value-of-program)

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defs body)
        (let
          ( [env (add-module-defns-to-env module-defs (empty-env))])
          (value-of body env)))
      ))
)

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
(define add-module-defns-to-env
  (lambda (module-defs env)
    (if (null? module-defs)
      env
      (cases module-definition (car module-defs)
        (a-module-definition (m-name iface sub-defns m-body)
          (let ( [new-env (add-module-defns-to-env sub-defns env)])
            (add-module-defns-to-env 
              (cdr module-defs)
              (extend-env-with-module
                m-name
                (value-of-module-body
                m-body new-env)
              env)))))))
)


;; value-of-module-body : ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module
          (defns-to-env defns env)))))
)


(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp)
          (let* 
            ( [val (value-of exp env)]
              [new-env (extend-env var val env)])
            (extend-env var val
              (defns-to-env (cdr defns) new-env)))))))
)

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
        (num-val num))
        
      (var-exp (var)
        (apply-env env var))
      
      (qualified-var-exp (m-name var-names)
        (let ( [val (lookup-qualified-var-in-env m-name (car var-names) env)])
          (if (null? (cdr var-names))
            val
            (let loop ( [var-names (cdr var-names)] [env (expval->module->bindings val)])
              (let ( [val (apply-env env (car var-names))])
                (if (null? (cdr var-names))
                  val
                  (loop (cdr var-names)
                    (expval->module->bindings val))))))))

      (diff-exp (exp1 exp2)
        (let 
          ( [num1 (expval->num (value-of exp1 env))]
            [num2 (expval->num (value-of exp2 env))])
          (num-val (- num1 num2))))

      (zero?-exp (exp1)
        (bool-val 
          (zero? (expval->num (value-of exp1 env)))))

      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))

      (let-exp (var exp1 body)
        (let* 
          ( [val1 (value-of exp1 env)]
            [new-env (extend-env var val1 env)])
          (value-of body new-env)))
      
      (proc-exp (bvar ty body)
        (proc-val
          (procedure bvar body env)))
      
      (call-exp (rator rand)          
        (let 
          ( (proc (expval->proc (value-of rator env)))
            (arg  (value-of rand env)))
	      (apply-procedure proc arg)))

       (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
          (value-of letrec-body
            (extend-env-recursively proc-name bvar proc-body env)))
      ))
)



(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var arg saved-env)))))
)